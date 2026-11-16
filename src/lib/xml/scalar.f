\ XML 1.0 scalar/entity decoding retains byte offsets in its input view.
require src/lib/prelude.f
require src/lib/utf8-scalar.f
require src/lib/string.f

package XML
public
\ XML scalar/parser errors: -9201..-9208 and -9212..-9217.
-9201 s" a caller's span is too short for what it must hold, or an output would pass the largest size"
   ERROR:CODE E-CAPACITY
-9202 s" a length, start or count is negative, past its input, or names bytes at a null address"
   ERROR:CODE E-RANGE
-9203 s" the document is not well-formed XML" ERROR:CODE E-MALFORMED
-9204 s" the input ended inside a construct or with an element still open" ERROR:CODE E-TRUNCATED
-9205 s" the input holds a byte sequence that is not valid UTF-8" ERROR:CODE E-UTF8
-9206 s" the input holds a character XML 1.0 does not allow" ERROR:CODE E-SCALAR
-9207 s" a character or entity reference is malformed, out of range or names no predefined entity"
   ERROR:CODE E-ENTITY
-9208 s" a name breaks XML namespaces: a malformed qualified name, an undeclared prefix, a reserved prefix or uri bound wrongly, a prefix bound to the empty uri, or two attributes with one expanded name"
   ERROR:CODE E-NAMESPACE
-9212 s" an XML reader word ran out of turn: on a failed reader, with no current name, start tag or open element"
   ERROR:FAULT E-STATE
-9213 s" the document holds a DOCTYPE, which the reader does not read" ERROR:CODE E-DTD
-9214 s" the document's encoding or XML declaration is not one the reader takes" ERROR:CODE E-ENCODING
-9215 s" an output span overlaps the input or the reader's storage" ERROR:CODE E-ALIAS

private
$7FFFFFFFFFFFFFFF constant MAX-SIZE
0 constant DECODE-TEXT
1 constant DECODE-ATTR
2 constant DECODE-RAW

\ A numeric address view is needed solely for extent and alias checks.
: BYTE-ADDRESS ( ^u8 -- n ) NULL-PTR BYTE-VIEW - ;

: SPAN-CHECK ( ^u8 n -- )
   {: source size:n :}
   size 0 < if E-RANGE throw then
   source BYTE-ADDRESS {: start:n :}
   start 0 < if E-RANGE throw then
   size 0 > start 0= and if E-RANGE throw then
   size MAX-SIZE start - > if E-RANGE throw then ;

: OVERLAP? ( ^u8 n ^u8 n -- bool )
   {: first first-len:n second second-len:n :}
   first-len 0= second-len 0= or if false exit then
   first BYTE-ADDRESS second BYTE-ADDRESS second-len + <
   second BYTE-ADDRESS first BYTE-ADDRESS first-len + < and ;

: BYTE@ ( ^u8 n n -- n )
   {: source size:n index:n :}
   index 0 < index size >= or if E-TRUNCATED throw then
   source index + c@ ;

: BYTES= ( ^u8 n ^u8 n -- bool )
   {: first first-len:n second second-len:n :}
   first-len second-len <> if false exit then
   first-len 0 ?do
      first i + c@ second i + c@ <> if false unloop exit then
   loop
   true ;

: XML-SPACE? ( n -- bool )
   dup $20 = over $9 = or over $A = or swap $D = or ;

: XML-SCALAR? ( n -- bool )
   {: scalar:n :}
   scalar $9 = scalar $A = or scalar $D = or
   scalar $20 >= scalar $D7FF <= and or
   scalar $E000 >= scalar $FFFD <= and or
   scalar $10000 >= scalar $10FFFF <= and or ;

: REQUIRE-SCALAR ( n -- )
   XML-SCALAR? 0= if E-SCALAR throw then ;

: ASCII-WIRE ( ^u8 n -- )
   {: source size:n :}
   size 0 ?do
      source i + c@ $7F > if E-ENCODING throw then
   loop ;

: SCALAR-AT ( ^u8 n n -- n n )
   {: source size:n cursor:n :}
   source size cursor BYTE@ drop
   source size cursor
   UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF ENDOF
      raw-byte OF 2drop E-UTF8 throw ENDOF
   ;MATCH
   {: scalar:n next:n :}
   scalar REQUIRE-SCALAR
   scalar next ;

: HEX-DIGIT ( n -- n )
   dup STR-HEX-DIGIT? 0= if E-ENTITY throw then
   STR-HEX-DIGIT-VALUE ;

: NUMERIC-STEP ( n n n -- n )
   {: value:n digit:n radix:n :}
   digit radix >= if E-ENTITY throw then
   value $10FFFF digit - radix / > if E-ENTITY throw then
   value radix * digit + ;

: NUMERIC-ENTITY ( ^u8 n n -- n n )
   {: source size:n start:n :}
   source size start BYTE@ $78 = if
      start 1+ $10
   else
      start 10
   then
   {: digits:n radix:n :}
   0 digits
   begin
      {: value:n cursor:n :}
      source size cursor BYTE@ $3B = if
         cursor digits = if E-ENTITY throw then
         value REQUIRE-SCALAR
         value cursor 1+ exit
      then
      source size cursor BYTE@ HEX-DIGIT {: digit:n :}
      value digit radix NUMERIC-STEP cursor 1+
   again ;

: NAMED-SCALAR ( ^u8 n -- n )
   {: source size:n :}
   source size s" lt" BYTES= if $3C exit then
   source size s" gt" BYTES= if $3E exit then
   source size s" amp" BYTES= if $26 exit then
   source size s" apos" BYTES= if $27 exit then
   source size s" quot" BYTES= if $22 exit then
   E-ENTITY throw ;

: SEMICOLON? ( n ^u8 n -- bool )
   {: cursor:n source size:n :}
   source size cursor BYTE@ $3B = ;

: ENTITY-AT ( ^u8 n n -- n n )
   {: source size:n start:n :}
   source size start 1+ BYTE@ $23 = if
      source size start 2 + NUMERIC-ENTITY exit
   then
   start 1+
   begin
      dup source size SEMICOLON? 0=
   while
      dup start - 5 > if E-ENTITY throw then
      1+
   repeat
   {: end:n :}
   source start 1+ + end start - 1- NAMED-SCALAR
   end 1+ ;

: NORMALIZED ( ^u8 n n n n -- n n )
   {: source size:n mode:n scalar:n next:n :}
   scalar $D = if
      next size < if
         source next + c@ $A = if next 1+ else next then
      else next then
      $A swap
   else scalar next then
   {: value:n after:n :}
   mode DECODE-ATTR = value XML-SPACE? and if
      $20 after
   else value after then ;

: DECODE-AT ( ^u8 n n n -- n n )
   {: source size:n cursor:n mode:n :}
   mode DECODE-RAW <> if
      source size cursor BYTE@ $26 = if
         source size cursor ENTITY-AT exit
      then
   then
   source size mode source size cursor SCALAR-AT NORMALIZED ;

: SCALAR-WIDTH ( n -- n )
   dup $80 < if drop 1 exit then
   dup $800 < if drop 2 exit then
   $10000 < if 3 else 4 then ;

: DECODED-SIZE ( ^u8 n n -- n )
   {: source size:n mode:n :}
   source size SPAN-CHECK
   0 0
   begin dup size < while
      {: total:n cursor:n :}
      source size cursor mode DECODE-AT
      {: scalar:n next:n :}
      total scalar SCALAR-WIDTH + next
   repeat
   drop ;

: PUT-BYTE ( n ^u8 -- ^u8 )
   {: byte:n destination :}
   byte destination c!
   destination 1+ ;

: PUT-CONTINUATION ( n ^u8 -- ^u8 )
   {: value:n destination :}
   value $3F and $80 or destination PUT-BYTE ;

: PUT-SCALAR ( n ^u8 -- ^u8 )
   {: scalar:n destination :}
   scalar $80 < if scalar destination PUT-BYTE exit then
   scalar $800 < if
      scalar scalar 6 rshift $C0 or destination PUT-BYTE
      PUT-CONTINUATION exit
   then
   scalar $10000 < if
      scalar 12 rshift $E0 or destination PUT-BYTE
   else
      scalar 18 rshift $F0 or destination PUT-BYTE
      {: next :}
      scalar 12 rshift next PUT-CONTINUATION
   then
   {: next :}
   scalar 6 rshift next PUT-CONTINUATION
   {: last :}
   scalar last PUT-CONTINUATION ;

: DECODE-WRITE ( ^u8 n n ^u8 -- )
   {: source size:n mode:n destination :}
   destination 0
   begin dup size < while
      {: next-output cursor:n :}
      source size cursor mode DECODE-AT
      {: scalar:n next:n :}
      scalar next-output PUT-SCALAR next
   repeat
   2drop ;

: DECODE-INTO ( ^u8 n n ^u8 n -- n )
   {: source size:n mode:n destination cap:n :}
   source size mode DECODED-SIZE {: needed:n :}
   destination cap SPAN-CHECK
   needed cap > if E-CAPACITY throw then
   source size destination cap OVERLAP? if E-ALIAS throw then
   source size mode destination DECODE-WRITE
   needed ;

: NORMAL-EQUAL? ( ^u8 n ^u8 n -- bool )
   {: first first-len:n second second-len:n :}
   0 0
   begin
      {: first-index:n second-index:n :}
      first-index first-len = second-index second-len = or if
         first-index first-len = second-index second-len = and exit
      then
      first first-len first-index DECODE-ATTR DECODE-AT
      {: first-scalar:n first-next:n :}
      second second-len second-index DECODE-ATTR DECODE-AT
      {: second-scalar:n second-next:n :}
      first-scalar second-scalar <> if false exit then
      first-next second-next
   again ;

;package
