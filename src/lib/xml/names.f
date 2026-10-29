\ names.f - XML names, namespace scopes and PREFIX-URI; part of package XML
\ (src/lib/xml.f). Name spans keep their prefix; URI and ATTR-URI answer the
\ resolved, decoded namespace URI. A default namespace applies to elements, not
\ to unprefixed attributes. Duplicate expanded attribute names and invalid
\ bindings are refused before the start event is returned.

require src/lib/xml/state.f
require src/lib/siphash.f

package XML
private
\ XML 1.0 Fifth Edition NameStartChar ranges, excluding ':' for NCNames.
15 constant NAME-RANGE-COUNT
NAME-RANGE-COUNT 2 * TYPED-BUFFER NAME-RANGES n

: RANGE! ( n n n -- )
   {: first:n last:n index:n :}
   first index 2 * NAME-RANGES !
   last index 2 * 1+ NAME-RANGES ! ;

$41 $5A 0 RANGE!
$5F $5F 1 RANGE!
$61 $7A 2 RANGE!
$C0 $D6 3 RANGE!
$D8 $F6 4 RANGE!
$F8 $2FF 5 RANGE!
$370 $37D 6 RANGE!
$37F $1FFF 7 RANGE!
$200C $200D 8 RANGE!
$2070 $218F 9 RANGE!
$2C00 $2FEF 10 RANGE!
$3001 $D7FF 11 RANGE!
$F900 $FDCF 12 RANGE!
$FDF0 $FFFD 13 RANGE!
$10000 $EFFFF 14 RANGE!

: NC-INITIAL? ( n -- bool )
   {: scalar:n :}
   NAME-RANGE-COUNT 0 ?do
      i 2 * {: index:n :}
      scalar index NAME-RANGES @ >=
      scalar index 1+ NAME-RANGES @ <= and if
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

: NAME-CONTINUES? ( ptr n -- bool )
   {: state :}
   state EOF? if false exit then
   state INPUT? if state INPUT-SCALAR-ENSURE then
   state SOURCE$ state POSITION + @ SCALAR-AT drop NAME-CHAR? ;

: SCAN-NAME ( ptr n -- n n )
   {: state :}
   state POSITION + @ {: start:n :}
   state SCAN-SCALAR dup NC-INITIAL? swap $3A = or 0= if
      E-MALFORMED throw
   then
   begin state NAME-CONTINUES? while state SCAN-SCALAR drop repeat
   start state POSITION + @ start - ;

: QNAME-CHECK ( ptr n n n -- )
   {: state start:n size:n :}
   state SOURCE$ start SCALAR-AT drop NC-INITIAL? 0= if
      E-NAMESPACE throw
   then
   0
   size 0 ?do
      state start i + AT $3A = if
         1+
         dup 1 > i size 1- = or if E-NAMESPACE throw then
         state SOURCE$ start i + 1+ SCALAR-AT drop NC-INITIAL? 0= if
            E-NAMESPACE throw
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
: KEY@ ( ptr n -- n n )
   {: state :}
   state HASH-K0 + @ state HASH-K1 + @ ;

: BYTES-HASH ( ptr n ptr u8 n -- n )
   {: state source size:n :}
   state KEY@ source size SIPHASH:BYTES ;

: HASH-START ( ptr n -- ptr n )
   {: state :}
   state SIP-STATE + {: sip :}
   sip state KEY@ SIPHASH:INIT
   sip ;

: BUCKET ( n n -- n ) {: hash:n count:n :} hash 1 rshift count mod ;

: QUOTE? ( n -- bool )
   dup $22 = swap $27 = or ;

: SCAN-VALUE ( ptr n -- n n )
   {: state :}
   state PEEK {: quote:n :}
   quote QUOTE? 0= if E-MALFORMED throw then
   state ADVANCE
   state POSITION + @ {: start:n :}
   begin state PEEK quote <> while
      state PEEK $3C = if E-MALFORMED throw then
      state DECODE-ATTR SCAN-DECODE
      state POSITION + ! drop
   repeat
   start state POSITION + @ start -
   state ADVANCE ;

: SAVE-ATTRIBUTE ( ptr n n n n n -- )
   {: state name:n name-size:n value:n value-size:n :}
   state state ATTR-COUNT + @ 1+ ATTRS-GROW
   state state ATTR-COUNT + @ ATTRIBUTE {: attr :}
   name attr !
   name-size attr CELL + !
   value attr 2 cells + !
   value-size attr 3 cells + !
   name attr 4 cells + !
   state POSITION + @ name - attr 5 cells + !
   0 attr 6 cells + !
   0 attr 7 cells + !
   1 state ATTR-COUNT + +! ;

: SCAN-ATTRIBUTE ( ptr n -- )
   {: state :}
   state SCAN-NAME {: name:n size:n :}
   state name size QNAME-CHECK
   state SKIP-WHITE
   state $3D REQUIRE-BYTE
   state SKIP-WHITE
   state name size state SCAN-VALUE SAVE-ATTRIBUTE ;

: ATTRIBUTE-NAME$ ( ptr n ptr n -- ptr u8 n )
   {: state attr :}
   state attr @ attr CELL + @ SPAN$ ;

: ATTRIBUTE-VALUE$ ( ptr n ptr n -- ptr u8 n )
   {: state attr :}
   state attr 2 cells + @ attr 3 cells + @ SPAN$ ;

: PREFIX-SIZE ( ptr u8 n -- n )
   {: source size:n :}
   size 0 ?do
      source i + c@ $3A = if i unloop exit then
   loop
   0 ;

: NAME-MATCH? ( ptr n ptr n ptr u8 n -- bool )
   {: state attr expected size:n :}
   state attr ATTRIBUTE-NAME$ expected size BYTES= ;

: NS-ATTRIBUTE? ( ptr n ptr n -- bool )
   {: state attr :}
   state attr s" xmlns" NAME-MATCH? if true exit then
   state attr ATTRIBUTE-NAME$ {: name size:n :}
   size 6 < if false exit then
   name 6 s" xmlns:" BYTES= ;

: NS-PREFIX ( ptr n ptr n -- n n )
   {: state attr :}
   state attr s" xmlns" NAME-MATCH? if 0 0 exit then
   attr @ 6 + attr CELL + @ 6 - ;

: RESERVED-URI? ( ptr n ptr n n -- bool )
   {: state attr uri:n :}
   state attr ATTRIBUTE-VALUE$
   state uri 0 URI$ NORMAL-EQUAL? ;

: NS-BINDING-CHECK ( ptr n ptr n -- )
   {: state attr :}
   state state attr NS-PREFIX SPAN$ {: prefix size:n :}
   prefix size s" xmlns" BYTES= if E-NAMESPACE throw then
   state attr XMLNS-URI RESERVED-URI? if E-NAMESPACE throw then
   prefix size s" xml" BYTES= if
      state attr XML-URI RESERVED-URI? 0= if E-NAMESPACE throw then
   else
      state attr XML-URI RESERVED-URI? if E-NAMESPACE throw then
   then
   size 0 > attr 3 cells + @ 0= and if E-NAMESPACE throw then ;

\ A URI hashes its decoded scalars, one word each: the units NORMAL-EQUAL?
\ compares, so URIs it calls equal hash equal however they are spelled.
: URI-HASH ( ptr n ptr u8 n -- n )
   {: state source size:n :}
   state HASH-START {: sip :}
   0
   begin dup size < while
      {: cursor:n :}
      source size cursor DECODE-ATTR DECODE-AT
      {: scalar:n next:n :}
      sip scalar SIPHASH:ABSORB-WORD next
   repeat
   drop
   sip SIPHASH:FINISH ;

\ An expanded name hashes its URI's hash, then its local name's bytes. Each
\ binding keeps its URI's hash, so an attribute costs its local name, not its
\ URI, and equal expanded names hash equal.
: EXPANDED-HASH ( ptr n n ptr u8 n -- n )
   {: state uri:n local size:n :}
   state HASH-START {: sip :}
   sip uri SIPHASH:ABSORB-WORD
   size 0 ?do sip local i + c@ SIPHASH:ABSORB-BYTE loop
   sip SIPHASH:FINISH ;

: EMPTY-URI-HASH ( ptr n -- n ) s" " URI-HASH ;

: PREFIX-HEAD ( ptr n ptr u8 n -- ptr n )
   {: state prefix size:n :}
   state state prefix size BYTES-HASH state NS-CAP + @ BUCKET
   NAMESPACE NS-HEAD + ;

: BINDING-PREFIX$ ( ptr n ptr n -- ptr u8 n )
   {: state binding :}
   state binding @ binding CELL + @ SPAN$ ;

: LINK-BINDING ( ptr n n -- )
   {: state index:n :}
   state index NAMESPACE {: binding :}
   state state binding BINDING-PREFIX$ PREFIX-HEAD {: head :}
   head @ binding NS-LINK + !
   index head ! ;


\ Make room for one more binding. The bucket count is the table's capacity, so
\ a growth relinks every live binding oldest first, keeping the newest of each
\ prefix its bucket's head as POP-BINDINGS requires.
: NS-ENSURE ( ptr n -- )
   {: state :}
   state NS-COUNT + @ 1+ state NS-CAP + @ <= if exit then
   state state NS-COUNT + @ 1+ NS-GROW
   state NS-HEADS-CLEAR
   state NS-COUNT + @ 0 ?do state i LINK-BINDING loop ;


: SAVE-NAMESPACE ( ptr n ptr n -- )
   {: state attr :}
   state attr NS-BINDING-CHECK
   state NS-ENSURE
   state NS-COUNT + @ {: index:n :}
   state index NAMESPACE {: binding :}
   state state attr NS-PREFIX SCOPE-REF
   binding CELL + ! binding !
   state attr 2 cells + @ attr 3 cells + @ SCOPE-REF
   binding 3 cells + ! binding 2 cells + !
   state state attr ATTRIBUTE-VALUE$ URI-HASH binding NS-HASH + !
   state index LINK-BINDING
   1 state NS-COUNT + +! ;

\ Drop the bindings above count, newest first: each is its bucket's head.
: POP-BINDINGS ( ptr n n -- )
   {: state count:n :}
   begin state NS-COUNT + @ count > while
      -1 state NS-COUNT + +!
      state state NS-COUNT + @ NAMESPACE {: binding :}
      binding NS-LINK + @
      state state binding BINDING-PREFIX$ PREFIX-HEAD !
   repeat ;

: CLEAR-ATTR-HEADS ( ptr n -- )
   {: state :}
   state ATTR-COUNT + @ 0 ?do -1 state i ATTRIBUTE ATTR-HEAD + ! loop ;

: ATTR-ENTER ( ptr n n n [ ptr n ptr n ptr n -- bool ] -- )
   {: state index:n hash:n same :}
   state index ATTRIBUTE {: attr :}
   hash attr ATTR-HASH + !
   state hash state ATTR-COUNT + @ BUCKET ATTRIBUTE ATTR-HEAD + {: head :}
   head @
   begin dup 0 >= while
      state swap ATTRIBUTE {: other :}
      other ATTR-HASH + @ hash = if
         state attr other same execute if E-NAMESPACE throw then
      then
      other ATTR-LINK + @
   repeat
   drop
   head @ attr ATTR-LINK + !
   index head ! ;

: SAME-RAW-NAME? ( ptr n ptr n ptr n -- bool )
   {: state first second :}
   state first ATTRIBUTE-NAME$ state second ATTRIBUTE-NAME$ BYTES= ;

: COLLECT-NAMESPACES ( ptr n -- )
   {: state :}
   state CLEAR-ATTR-HEADS
   state ATTR-COUNT + @ 0 ?do
      state i ATTRIBUTE {: attr :}
      state state attr ATTRIBUTE-NAME$ BYTES-HASH {: hash:n :}
      state i hash [: SAME-RAW-NAME? ;] ATTR-ENTER
      state attr NS-ATTRIBUTE? if state attr SAVE-NAMESPACE then
   loop ;

: BINDING-INDEX ( ptr n ptr u8 n -- n )
   {: state prefix size:n :}
   state prefix size PREFIX-HEAD @
   begin dup 0 >= while
      state over NAMESPACE {: binding :}
      prefix size state binding BINDING-PREFIX$ BYTES= if exit then
      drop binding NS-LINK + @
   repeat ;

: PREFIX-BINDING ( ptr n ptr u8 n -- n n n bool )
   {: state prefix size:n :}
   prefix size s" xml" BYTES= if
      XML-URI 0 state state XML-URI 0 URI$ URI-HASH true exit
   then
   prefix size s" xmlns" BYTES= if 0 0 0 false exit then
   state prefix size BINDING-INDEX {: index:n :}
   index 0 >= if
      state index NAMESPACE {: binding :}
      binding 2 cells + @ binding 3 cells + @ binding NS-HASH + @ true exit
   then
   0 0 state EMPTY-URI-HASH size 0= ;

: BOUND-URI ( ptr n ptr u8 n -- n n bool )
   PREFIX-BINDING {: start:n size:n hash:n bound:bool :}
   start size bound ;

: LOOKUP-URI ( ptr n n n -- n n n )
   {: state prefix:n size:n :}
   state state prefix size SPAN$ PREFIX-BINDING if exit then
   E-NAMESPACE throw ;

: RESOLVE-NAME ( ptr n n n bool -- n n n )
   {: state name:n size:n attr:bool :}
   state name size SPAN$ PREFIX-SIZE {: prefix-size:n :}
   prefix-size 0= attr and if 0 0 state EMPTY-URI-HASH exit then
   state name prefix-size SPAN$ s" xmlns" BYTES= if E-NAMESPACE throw then
   state name prefix-size LOOKUP-URI ;

: RESOLVE-ATTRIBUTE ( ptr n ptr n -- n )
   {: state attr :}
   state attr NS-ATTRIBUTE? if
      XMLNS-URI 0 state state XMLNS-URI 0 URI$ URI-HASH
   else
      state attr @ attr CELL + @ true RESOLVE-NAME
   then
   {: start:n size:n hash:n :}
   start attr 6 cells + !
   size attr 7 cells + !
   hash ;

: ATTRIBUTE-URI$ ( ptr n ptr n -- ptr u8 n )
   {: state attr :}
   state attr 6 cells + @ attr 7 cells + @ URI$ ;

: SAME-EXPANDED-NAME? ( ptr n ptr n ptr n -- bool )
   {: state first second :}
   state first ATTRIBUTE-NAME$ LOCAL-SPAN
   state second ATTRIBUTE-NAME$ LOCAL-SPAN BYTES= 0= if false exit then
   state first ATTRIBUTE-URI$ state second ATTRIBUTE-URI$ NORMAL-EQUAL? ;

: RESOLVE-ATTRIBUTES ( ptr n -- )
   {: state :}
   state CLEAR-ATTR-HEADS
   state ATTR-COUNT + @ 0 ?do
      state i ATTRIBUTE {: attr :}
      state state attr RESOLVE-ATTRIBUTE
      state attr ATTRIBUTE-NAME$ LOCAL-SPAN EXPANDED-HASH {: hash:n :}
      state i hash [: SAME-EXPANDED-NAME? ;] ATTR-ENTER
   loop ;

: RESOLVE-ELEMENT ( ptr n -- )
   {: state :}
   state COLLECT-NAMESPACES
   state state NAME-OFF + @ state NAME-LEN + @ false RESOLVE-NAME drop
   state URI-LEN + ! state URI-OFF + !
   state RESOLVE-ATTRIBUTES ;

: PREFIX-URI-ARGS ( XML:reader ptr u8 n ptr u8 n -- XML:reader ptr n ptr u8 n ptr u8 n )
   {: prefix size:n destination cap:n :}
   STATE prefix size destination cap ;

: PREFIX-URI-STATE ( ptr n ptr u8 n ptr u8 n -- n bool )
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
: PREFIX-URI ( XML:reader ptr u8 n ptr u8 n -- XML:reader n bool )
   PREFIX-URI-ARGS PREFIX-URI-STATE ;

;package
