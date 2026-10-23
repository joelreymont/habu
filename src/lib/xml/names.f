\ names.f - XML names, namespace scopes and PREFIX-URI.
\
\ Name spans keep their prefix; URI and ATTR-URI answer the resolved, decoded
\ namespace URI. A default namespace applies to elements, not to unprefixed
\ attributes. Duplicate expanded attribute names and invalid bindings are
\ refused before the start event is returned. PREFIX-URI resolves a decoded
\ prefix against the current scope, the start tag's own declarations included,
\ into caller storage and answers the count and true when bound; an unknown
\ nonempty prefix, xmlns included, answers 0 and false without writing, `xml`
\ its implicit URI, and the empty prefix the default (zero bytes when none).
\ The scope lasts through the element's end event; before the root, past its
\ end, at EOF or on a failed reader the lookup is E-STATE. A short or aliasing
\ output throws and leaves the reader unchanged. Part of package XML
\ (src/lib/xml.f).

require src/lib/xml/state.f

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
   state ATTR-COUNT + @ state CAPACITY + @ >= if E-ATTRIBUTES throw then
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

: SAVE-NAMESPACE ( ptr n ptr n -- )
   {: state attr :}
   state NS-COUNT + @ state CAPACITY + @ >= if E-NAMESPACES throw then
   state attr NS-BINDING-CHECK
   state state NS-COUNT + @ NAMESPACE {: binding :}
   state attr NS-PREFIX
   binding CELL + ! binding !
   attr 2 cells + @ binding 2 cells + !
   attr 3 cells + @ binding 3 cells + !
   1 state NS-COUNT + +! ;

: DUPLICATE-NAME-CHECK ( ptr n n -- )
   {: state index:n :}
   state index ATTRIBUTE {: attr :}
   index 0 ?do
      state attr ATTRIBUTE-NAME$
      state state i ATTRIBUTE ATTRIBUTE-NAME$ BYTES= if E-NAMESPACE throw then
   loop ;

: COLLECT-NAMESPACES ( ptr n -- )
   {: state :}
   state ATTR-COUNT + @ 0 ?do
      state i DUPLICATE-NAME-CHECK
      state i ATTRIBUTE {: attr :}
      state attr NS-ATTRIBUTE? if state attr SAVE-NAMESPACE then
   loop ;

: BOUND-URI ( ptr n ptr u8 n -- n n bool )
   {: state prefix size:n :}
   prefix size s" xml" BYTES= if XML-URI 0 true exit then
   prefix size s" xmlns" BYTES= if 0 0 false exit then
   state NS-COUNT + @
   begin dup 0 > while
      1-
      state over NAMESPACE {: binding :}
      prefix size state binding @ binding CELL + @ SPAN$ BYTES= if
         drop binding 2 cells + @ binding 3 cells + @ true exit
      then
   repeat
   drop
   0 0 size 0= ;

: LOOKUP-URI ( ptr n n n -- n n )
   {: state prefix:n size:n :}
   state state prefix size SPAN$ BOUND-URI if exit then
   E-NAMESPACE throw ;

: RESOLVE-NAME ( ptr n n n bool -- n n )
   {: state name:n size:n attr:bool :}
   state name size SPAN$ PREFIX-SIZE {: prefix-size:n :}
   prefix-size 0= attr and if 0 0 exit then
   state name prefix-size SPAN$ s" xmlns" BYTES= if E-NAMESPACE throw then
   state name prefix-size LOOKUP-URI ;

: RESOLVE-ATTRIBUTE ( ptr n ptr n -- )
   {: state attr :}
   state attr NS-ATTRIBUTE? if
      XMLNS-URI 0
   else
      state attr @ attr CELL + @ true RESOLVE-NAME
   then
   attr 7 cells + ! attr 6 cells + ! ;

: ATTRIBUTE-URI$ ( ptr n ptr n -- ptr u8 n )
   {: state attr :}
   state attr 6 cells + @ attr 7 cells + @ URI$ ;

: SAME-EXPANDED-NAME? ( ptr n ptr n ptr n -- bool )
   {: state first second :}
   state first ATTRIBUTE-NAME$ LOCAL-SPAN
   state second ATTRIBUTE-NAME$ LOCAL-SPAN BYTES= 0= if false exit then
   state first ATTRIBUTE-URI$ state second ATTRIBUTE-URI$ NORMAL-EQUAL? ;

: EXPANDED-NAME-CHECK ( ptr n n -- )
   {: state index:n :}
   state index ATTRIBUTE {: attr :}
   index 0 ?do
      state attr state i ATTRIBUTE SAME-EXPANDED-NAME? if
         E-NAMESPACE throw
      then
   loop ;

: RESOLVE-ATTRIBUTES ( ptr n -- )
   {: state :}
   state ATTR-COUNT + @ 0 ?do
      state state i ATTRIBUTE RESOLVE-ATTRIBUTE
      state i EXPANDED-NAME-CHECK
   loop ;

: RESOLVE-ELEMENT ( ptr n -- )
   {: state :}
   state COLLECT-NAMESPACES
   state state NAME-OFF + @ state NAME-LEN + @ false RESOLVE-NAME
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
      state -rot URI$ DECODE-ATTR destination cap DECODE-INTO true
   else
      2drop 0 false
   then ;

public
: PREFIX-URI ( XML:reader ptr u8 n ptr u8 n -- XML:reader n bool )
   PREFIX-URI-ARGS PREFIX-URI-STATE ;

;package
