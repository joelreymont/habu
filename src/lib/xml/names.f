split-str
\ names.f - XML names, namespace scopes and PREFIX-URI; part of package XML
\ (src/lib/xml.f). Name spans keep their prefix; URI and ATTR-URI answer the
\ resolved, decoded namespace URI. A default namespace applies to elements, not
\ to unprefixed attributes. Duplicate expanded attribute names and invalid
\ bindings are refused before the start event is returned.

require src/lib/xml/state.f
require src/lib/siphash.f

package XML
private
\ XML 1.0 Fifth Edition NameStartChar ranges, excluding ':' for NCNames: pairs
\ of first and last scalar.
create NAME-RANGES
   $41 ,     $5A ,
   $5F ,     $5F ,
   $61 ,     $7A ,
   $C0 ,     $D6 ,
   $D8 ,     $F6 ,
   $F8 ,     $2FF ,
   $370 ,    $37D ,
   $37F ,    $1FFF ,
   $200C ,   $200D ,
   $2070 ,   $218F ,
   $2C00 ,   $2FEF ,
   $3001 ,   $D7FF ,
   $F900 ,   $FDCF ,
   $FDF0 ,   $FFFD ,
   $10000 ,  $EFFFF ,
here NAME-RANGES - 2 cells / constant NAME-RANGES#

: NC-INITIAL? ( n -- bool )
   {: scalar:n :}
   NAME-RANGES# 0 ?do
      scalar NAME-RANGES i 2 * cells + @ >=
      scalar NAME-RANGES i 2 * 1+ cells + @ <= and if
         true unloop exit
      then
   loop
   false ;

: NAME-CHAR? ( n -- bool )
   {: scalar:n :}
   scalar NC-INITIAL? scalar $3A = or
   scalar $2D = or scalar $2E = or
   scalar $30 >= scalar $39 <= and or
   scalar $B7 = or
   scalar $300 >= scalar $36F <= and or
   scalar $203F >= scalar $2040 <= and or ;

: NAME-CONTINUES? ( ^n -- bool )
   {: state :}
   state EOF? if false exit then
   state INPUT? if state INPUT-SCALAR-ENSURE then
   state SOURCE$ state WINDOW-PLACE state POSITION + @ SCALAR-AT
   drop NAME-CHAR? ;

: SCAN-NAME ( ^n -- n n )
   {: state :}
   state POSITION + @ {: start:n :}
   state SCAN-SCALAR dup NC-INITIAL? swap $3A = or 0= if
      state start s" a name" s" start the name with a letter, `_` or `:`"
      E-MALFORMED REJECT
   then
   begin state NAME-CONTINUES? while state SCAN-SCALAR drop repeat
   start state POSITION + @ start - ;

: QNAME-CHECK ( ^n n n -- )
   {: state start:n size:n :}
   state SOURCE$ state WINDOW-PLACE start SCALAR-AT drop
   NC-INITIAL? 0= if
      state start s" a name that starts with a letter or `_`"
      s" write a prefix before the leading `:`, or remove it" E-NAMESPACE REJECT
   then
   0
   size 0 ?do
      state start i + AT $3A = if
         1+
         dup 1 > i size 1- = or if
            state start s" a name with at most one `:`, between a prefix and a local name"
            s" write the name as `prefix:local` or `local`" E-NAMESPACE REJECT
         then
         state SOURCE$ state WINDOW-PLACE start i + 1+ SCALAR-AT
         drop NC-INITIAL? 0= if
            state start s" a local name after `:` that starts with a letter or `_`"
            s" start the part after `:` with a letter or `_`" E-NAMESPACE REJECT
         then
      then
   loop
   drop ;

\ One tag's attributes and the live bindings are hashed into buckets, so the
\ duplicate checks and each prefix lookup compare only within one bucket. The
\ hash is SipHash-2-4 under the reader's secret key, drawn per reader at INIT,
\ so a document cannot aim its names at one bucket: an element is expected to
\ cost time linear in its attributes and bindings. That is an expectation over
\ the key, not a worst-case guarantee. A collision costs only time: a full-hash
\ match is confirmed by comparing the names.
: KEY@ ( ^n -- n n )
   {: state :}
   state HASH-K0 + @ state HASH-K1 + @ ;

: BYTES-HASH ( ^n str -- n )
   {: state source size:n :}
   state KEY@ source size SIPHASH:BYTES ;

: HASH-START ( ^n -- ^n )
   {: state :}
   state SIP-STATE + {: sip :}
   sip state KEY@ SIPHASH:INIT
   sip ;

: BUCKET ( n n -- n ) {: hash:n count:n :} hash 1 rshift count mod ;

: QUOTE? ( n -- bool )
   dup $22 = swap $27 = or ;

: SCAN-VALUE ( ^n -- n n )
   {: state :}
   state PEEK {: quote:n :}
   quote QUOTE? 0= if
      state state POSITION + @ s\" a value in `\"` or `'` quotes"
      s\" write the value between `\"` or `'` quotes" E-MALFORMED REJECT
   then
   state ADVANCE
   state POSITION + @ {: start:n :}
   begin state PEEK quote <> while
      state PEEK $3C = if
         state state POSITION + @ s" the attribute value's closing quote"
         s" close the value with its quote, or write `&lt;` for a literal `<`"
         E-MALFORMED REJECT
      then
      state DECODE-ATTR SCAN-DECODE
      state POSITION + ! drop
   repeat
   start state POSITION + @ start -
   state ADVANCE ;

: SAVE-ATTRIBUTE ( ^n n n n n -- )
   {: state name:n name-size:n value:n value-size:n :}
   state state ATTR-COUNT + @ 1+ ATTRS-GROW
   state state ATTR-COUNT + @ ATTRIBUTE {: attr :}
   name attr ATTRIBUTE-NAME + !
   name-size attr ATTRIBUTE-NAME-U + !
   value attr ATTRIBUTE-VALUE + !
   value-size attr ATTRIBUTE-VALUE-U + !
   name attr ATTRIBUTE-RAW + !
   state POSITION + @ name - attr ATTRIBUTE-RAW-U + !
   0 attr ATTRIBUTE-URI + !
   0 attr ATTRIBUTE-URI-U + !
   1 state ATTR-COUNT + +! ;

: SCAN-ATTRIBUTE ( ^n -- )
   {: state :}
   state SCAN-NAME {: name:n size:n :}
   state name size QNAME-CHECK
   state SKIP-WHITE
   state $3D REQUIRE-BYTE
   state SKIP-WHITE
   state name size state SCAN-VALUE SAVE-ATTRIBUTE ;

: ATTRIBUTE-NAME$ ( ^n ^n -- str )
   {: state attr :}
   state attr ATTRIBUTE-NAME + @ attr ATTRIBUTE-NAME-U + @ SPAN$ ;

: ATTRIBUTE-VALUE$ ( ^n ^n -- str )
   {: state attr :}
   state attr ATTRIBUTE-VALUE + @ attr ATTRIBUTE-VALUE-U + @ SPAN$ ;

: PREFIX-SIZE ( str -- n )
   {: source size:n :}
   size 0 ?do
      source i + c@ $3A = if i unloop exit then
   loop
   0 ;

: NAME-MATCH? ( ^n ^n str -- bool )
   {: state attr expected size:n :}
   state attr ATTRIBUTE-NAME$ expected size BYTES= ;

: NS-ATTRIBUTE? ( ^n ^n -- bool )
   {: state attr :}
   state attr s" xmlns" NAME-MATCH? if true exit then
   state attr ATTRIBUTE-NAME$ {: name size:n :}
   size 6 < if false exit then
   name 6 s" xmlns:" BYTES= ;

: NS-PREFIX ( ^n ^n -- n n )
   {: state attr :}
   state attr s" xmlns" NAME-MATCH? if 0 0 exit then
   attr ATTRIBUTE-NAME + @ 6 + attr ATTRIBUTE-NAME-U + @ 6 - ;

: RESERVED-URI? ( ^n ^n n -- bool )
   {: state attr uri:n :}
   state attr ATTRIBUTE-VALUE$
   state uri 0 URI$ NORMAL-EQUAL? ;

: XMLNS-CHECK ( ^n ^n str -- )
   {: state attr prefix size:n :}
   attr ATTRIBUTE-NAME + @ {: at:n :}
   prefix size s" xmlns" BYTES= if
      state at s" a prefix other than `xmlns`"
      s" remove the declaration: XML binds `xmlns` itself"
      E-NAMESPACE REJECT
   then
   state attr XMLNS-URI RESERVED-URI? if
      state at
      s" a namespace other than `http://www.w3.org/2000/xmlns/`"
      s" declare another namespace URI, or remove the declaration"
      E-NAMESPACE REJECT
   then ;

: XML-PREFIX-CHECK ( ^n ^n str -- )
   {: state attr prefix size:n :}
   attr ATTRIBUTE-NAME + @ {: at:n :}
   prefix size s" xml" BYTES= if
      state attr XML-URI RESERVED-URI? 0= if
         state at
         s" `xml` bound to `http://www.w3.org/XML/1998/namespace`"
         s" remove the declaration: XML binds `xml` itself"
         E-NAMESPACE REJECT
      then
   else
      state attr XML-URI RESERVED-URI? if
         state at
         s" a namespace other than `http://www.w3.org/XML/1998/namespace`"
         s" write the `xml` prefix for that namespace, and remove this declaration"
         E-NAMESPACE REJECT
      then
   then ;

: NS-BINDING-CHECK ( ^n ^n -- )
   {: state attr :}
   state state attr NS-PREFIX SPAN$ {: prefix size:n :}
   state attr prefix size XMLNS-CHECK
   state attr prefix size XML-PREFIX-CHECK
   size 0 > attr ATTRIBUTE-VALUE-U + @ 0= and if
      state attr ATTRIBUTE-NAME + @ s" a prefix declaration with a URI"
      s" write the prefix's URI: XML 1.0 cannot undeclare a prefix"
      E-NAMESPACE REJECT
   then ;

\ A URI hashes its decoded scalars, one word each: the units NORMAL-EQUAL?
\ compares, so URIs it calls equal hash equal however they are spelled.
: URI-HASH ( ^n str -- n )
   {: state source size:n :}
   state HASH-START {: sip :}
   0
   begin dup size < while
      {: cursor:n :}
      source size WHOLE cursor DECODE-ATTR DECODE-AT
      {: scalar:n next:n :}
      sip scalar SIPHASH:ABSORB-WORD next
   repeat
   drop
   sip SIPHASH:FINISH ;

\ An expanded name hashes its URI's hash, then its local name's bytes. Each
\ binding keeps its URI's hash, so an attribute costs its local name, not its
\ URI, and equal expanded names hash equal.
: EXPANDED-HASH ( ^n n str -- n )
   {: state uri:n local size:n :}
   state HASH-START {: sip :}
   sip uri SIPHASH:ABSORB-WORD
   size 0 ?do sip local i + c@ SIPHASH:ABSORB-BYTE loop
   sip SIPHASH:FINISH ;

: EMPTY-URI-HASH ( ^n -- n ) s" " URI-HASH ;

: PREFIX-HEAD ( ^n str -- ^n )
   {: state prefix size:n :}
   state state prefix size BYTES-HASH state NS-CAP + @ BUCKET
   NAMESPACE BINDING-HEAD + ;

: BINDING-PREFIX$ ( ^n ^n -- str )
   {: state binding :}
   state binding BINDING-PREFIX + @ binding BINDING-PREFIX-U + @ SPAN$ ;

: LINK-BINDING ( ^n n -- )
   {: state index:n :}
   state index NAMESPACE {: binding :}
   state state binding BINDING-PREFIX$ PREFIX-HEAD {: head :}
   head @ binding BINDING-LINK + !
   index head ! ;


\ Make room for one more binding. The bucket count is the table's capacity, so
\ a growth relinks every live binding oldest first, keeping the newest of each
\ prefix its bucket's head as POP-BINDINGS requires.
: NS-ENSURE ( ^n -- )
   {: state :}
   state NS-COUNT + @ 1+ state NS-CAP + @ <= if exit then
   state state NS-COUNT + @ 1+ NS-GROW
   state NS-HEADS-CLEAR
   state NS-COUNT + @ 0 ?do state i LINK-BINDING loop ;


: SAVE-NAMESPACE ( ^n ^n -- )
   {: state attr :}
   state attr NS-BINDING-CHECK
   state NS-ENSURE
   state NS-COUNT + @ {: index:n :}
   state index NAMESPACE {: binding :}
   state state attr NS-PREFIX SCOPE-REF
   binding BINDING-PREFIX-U + ! binding BINDING-PREFIX + !
   state attr ATTRIBUTE-VALUE + @ attr ATTRIBUTE-VALUE-U + @ SCOPE-REF
   binding BINDING-URI-U + ! binding BINDING-URI + !
   state state attr ATTRIBUTE-VALUE$ URI-HASH binding BINDING-HASH + !
   state index LINK-BINDING
   1 state NS-COUNT + +! ;

\ Drop the bindings above count, newest first: each is its bucket's head.
: POP-BINDINGS ( ^n n -- )
   {: state count:n :}
   begin state NS-COUNT + @ count > while
      -1 state NS-COUNT + +!
      state state NS-COUNT + @ NAMESPACE {: binding :}
      binding BINDING-LINK + @
      state state binding BINDING-PREFIX$ PREFIX-HEAD !
   repeat ;

: CLEAR-ATTR-HEADS ( ^n -- )
   {: state :}
   state ATTR-COUNT + @ 0 ?do -1 state i ATTRIBUTE ATTRIBUTE-HEAD + ! loop ;

: ATTR-ENTER ( ^n n n [ ^n ^n ^n -- bool ] -- )
   {: state index:n hash:n same :}
   state index ATTRIBUTE {: attr :}
   hash attr ATTRIBUTE-HASH + !
   state hash state ATTR-COUNT + @ BUCKET ATTRIBUTE ATTRIBUTE-HEAD + {: head :}
   head @
   begin dup 0 >= while
      state swap ATTRIBUTE {: other :}
      other ATTRIBUTE-HASH + @ hash = if
         state attr other same execute if
            state attr ATTRIBUTE-NAME + @
            s" an attribute the element does not already have"
            s" remove the repeated attribute, or give it another name or namespace"
            E-NAMESPACE REJECT
         then
      then
      other ATTRIBUTE-LINK + @
   repeat
   drop
   head @ attr ATTRIBUTE-LINK + !
   index head ! ;

: SAME-RAW-NAME? ( ^n ^n ^n -- bool )
   {: state first second :}
   state first ATTRIBUTE-NAME$ state second ATTRIBUTE-NAME$ BYTES= ;

: COLLECT-NAMESPACES ( ^n -- )
   {: state :}
   state CLEAR-ATTR-HEADS
   state ATTR-COUNT + @ 0 ?do
      state i ATTRIBUTE {: attr :}
      state state attr ATTRIBUTE-NAME$ BYTES-HASH {: hash:n :}
      state i hash [: SAME-RAW-NAME? ;] ATTR-ENTER
      state attr NS-ATTRIBUTE? if state attr SAVE-NAMESPACE then
   loop ;

: BINDING-INDEX ( ^n str -- n )
   {: state prefix size:n :}
   state prefix size PREFIX-HEAD @
   begin dup 0 >= while
      state over NAMESPACE {: binding :}
      prefix size state binding BINDING-PREFIX$ BYTES= if exit then
      drop binding BINDING-LINK + @
   repeat ;

: PREFIX-BINDING ( ^n str -- n n n bool )
   {: state prefix size:n :}
   prefix size s" xml" BYTES= if
      XML-URI 0 state state XML-URI 0 URI$ URI-HASH true exit
   then
   prefix size s" xmlns" BYTES= if 0 0 0 false exit then
   state prefix size BINDING-INDEX {: index:n :}
   index 0 >= if
      state index NAMESPACE {: binding :}
      binding BINDING-URI + @ binding BINDING-URI-U + @ binding BINDING-HASH + @ true exit
   then
   0 0 state EMPTY-URI-HASH size 0= ;

: BOUND-URI ( ^n str -- n n bool )
   PREFIX-BINDING {: start:n size:n hash:n bound:bool :}
   start size bound ;

: LOOKUP-URI ( ^n n n -- n n n )
   {: state prefix:n size:n :}
   state state prefix size SPAN$ PREFIX-BINDING if exit then
   state prefix s" a prefix an `xmlns:` declaration binds in scope"
   s" declare it with `xmlns:` on this element or an enclosing one"
   E-NAMESPACE REJECT ;

: RESOLVE-NAME ( ^n n n bool -- n n n )
   {: state name:n size:n attr:bool :}
   state name size SPAN$ PREFIX-SIZE {: prefix-size:n :}
   prefix-size 0= attr and if 0 0 state EMPTY-URI-HASH exit then
   state name prefix-size SPAN$ s" xmlns" BYTES= if
      state name s" an element name without the `xmlns` prefix"
      s" rename the element: `xmlns` marks only namespace declarations"
      E-NAMESPACE REJECT
   then
   state name prefix-size LOOKUP-URI ;

: RESOLVE-ATTRIBUTE ( ^n ^n -- n )
   {: state attr :}
   state attr NS-ATTRIBUTE? if
      XMLNS-URI 0 state state XMLNS-URI 0 URI$ URI-HASH
   else
      state attr ATTRIBUTE-NAME + @ attr ATTRIBUTE-NAME-U + @ true RESOLVE-NAME
   then
   {: start:n size:n hash:n :}
   start attr ATTRIBUTE-URI + !
   size attr ATTRIBUTE-URI-U + !
   hash ;

: ATTRIBUTE-URI$ ( ^n ^n -- str )
   {: state attr :}
   state attr ATTRIBUTE-URI + @ attr ATTRIBUTE-URI-U + @ URI$ ;

: SAME-EXPANDED-NAME? ( ^n ^n ^n -- bool )
   {: state first second :}
   state first ATTRIBUTE-NAME$ LOCAL-SPAN
   state second ATTRIBUTE-NAME$ LOCAL-SPAN BYTES= 0= if false exit then
   state first ATTRIBUTE-URI$ state second ATTRIBUTE-URI$ NORMAL-EQUAL? ;

: RESOLVE-ATTRIBUTES ( ^n -- )
   {: state :}
   state CLEAR-ATTR-HEADS
   state ATTR-COUNT + @ 0 ?do
      state i ATTRIBUTE {: attr :}
      state state attr RESOLVE-ATTRIBUTE
      state attr ATTRIBUTE-NAME$ LOCAL-SPAN EXPANDED-HASH {: hash:n :}
      state i hash [: SAME-EXPANDED-NAME? ;] ATTR-ENTER
   loop ;

: RESOLVE-ELEMENT ( ^n -- )
   {: state :}
   state COLLECT-NAMESPACES
   state state NAME-OFF + @ state NAME-LEN + @ false RESOLVE-NAME drop
   state URI-LEN + ! state URI-OFF + !
   state RESOLVE-ATTRIBUTES ;

: PREFIX-URI-ARGS ( XML:reader str ^u8 n -- XML:reader ^n str ^u8 n )
   {: prefix size:n destination cap:n :}
   STATE prefix size destination cap ;

: PREFIX-URI-STATE ( ^n str ^u8 n -- n bool )
   {: state prefix size:n destination cap:n :}
   state LIVE
   state OPEN-DEPTH + @ 0= if E-STATE throw then
   prefix size SPAN-CHECK
   state destination cap OUTPUT-CHECK
   state prefix size BOUND-URI if
      state -rot URI$ destination cap URI-INTO true
   else
      2drop 0 false
   then ;

public
\ Resolves against the current scope, the start tag's own declarations
\ included. An unknown nonempty prefix, xmlns included, answers 0 and false
\ without writing, `xml` its implicit URI, the empty prefix the default (zero
\ bytes when none). Before the root, past its end, at EOF or on a failed reader
\ it is E-STATE; a short or aliasing output throws and leaves the reader alone.
: PREFIX-URI ( XML:reader str ^u8 n -- XML:reader n bool )
   PREFIX-URI-ARGS PREFIX-URI-STATE ;

;package
