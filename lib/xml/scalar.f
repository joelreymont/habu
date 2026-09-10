\ XML 1.0 scalar/entity decoding retains byte offsets into the original input.
require lib/prelude.f
require lib/utf8-scalar.f

package XML
public
\ XML scalar/parser errors: -9200..-9215.
-9200 constant E-STORAGE
-9201 constant E-CAPACITY
-9202 constant E-RANGE
-9203 constant E-MALFORMED
-9204 constant E-TRUNCATED
-9205 constant E-UTF8
-9206 constant E-SCALAR
-9207 constant E-ENTITY
-9208 constant E-NAMESPACE
-9209 constant E-DEPTH
-9210 constant E-ATTRIBUTES
-9211 constant E-NAMESPACES
-9212 constant E-STATE
-9213 constant E-DTD
-9214 constant E-ENCODING
-9215 constant E-ALIAS

private
$7FFFFFFFFFFFFFFF constant MAX-SIZE
0 constant DECODE-TEXT
1 constant DECODE-ATTR
2 constant DECODE-RAW

\ A numeric address view is needed solely for extent and alias checks.
\ Retirement owner: cap:raw-pointer-lifetime. Exercised by xml-test.f.
TRUSTED: BYTE-ADDRESS ( ptr u8 -- n ) ;

: SPAN-CHECK ( ptr u8 n -- )
   {: source size:n :}
   size 0 < if E-RANGE throw then
   source BYTE-ADDRESS {: start:n :}
   start 0 < if E-RANGE throw then
   size 0 > start 0= and if E-RANGE throw then
   size MAX-SIZE start - > if E-RANGE throw then ;

: OVERLAP? ( ptr u8 n ptr u8 n -- bool )
   {: first first-len:n second second-len:n :}
   first-len 0= second-len 0= or if false exit then
   first BYTE-ADDRESS second BYTE-ADDRESS second-len + <
   second BYTE-ADDRESS first BYTE-ADDRESS first-len + < and ;

: BYTE@ ( ptr u8 n n -- n )
   {: source size:n index:n :}
   index 0 < index size >= or if E-TRUNCATED throw then
   source index + c@ ;

: BYTES= ( ptr u8 n ptr u8 n -- bool )
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

: SCALAR-AT ( ptr u8 n n -- n n )
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
   {: byte:n :}
   byte $30 >= byte $39 <= and if byte $30 - exit then
   byte $61 >= byte $66 <= and if byte $61 - 10 + exit then
   byte $41 >= byte $46 <= and if byte $41 - 10 + exit then
   E-ENTITY throw ;

: NUMERIC-ENTITY ( ptr u8 n n -- n n )
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
      digit radix >= if E-ENTITY throw then
      value $10FFFF digit - radix / > if E-ENTITY throw then
      value radix * digit + cursor 1+
   again ;

: NAMED-SCALAR ( ptr u8 n -- n )
   {: source size:n :}
   source size s" lt" BYTES= if $3C exit then
   source size s" gt" BYTES= if $3E exit then
   source size s" amp" BYTES= if $26 exit then
   source size s" apos" BYTES= if $27 exit then
   source size s" quot" BYTES= if $22 exit then
   E-ENTITY throw ;

: SEMICOLON? ( n ptr u8 n -- bool )
   {: cursor:n source size:n :}
   source size cursor BYTE@ $3B = ;

: ENTITY-AT ( ptr u8 n n -- n n )
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

: NORMALIZED ( ptr u8 n n n n -- n n )
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

: DECODE-AT ( ptr u8 n n n -- n n )
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

: DECODED-SIZE ( ptr u8 n n -- n )
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

: PUT-BYTE ( n ptr u8 -- ptr u8 )
   {: byte:n destination :}
   byte destination c!
   destination 1+ ;

: PUT-CONTINUATION ( n ptr u8 -- ptr u8 )
   {: value:n destination :}
   value $3F and $80 or destination PUT-BYTE ;

: PUT-SCALAR ( n ptr u8 -- ptr u8 )
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

: DECODE-WRITE ( ptr u8 n n ptr u8 -- )
   {: source size:n mode:n destination :}
   destination 0
   begin dup size < while
      {: next-output cursor:n :}
      source size cursor mode DECODE-AT
      {: scalar:n next:n :}
      scalar next-output PUT-SCALAR next
   repeat
   2drop ;

: DECODE-INTO ( ptr u8 n n ptr u8 n -- n )
   {: source size:n mode:n destination cap:n :}
   source size mode DECODED-SIZE {: needed:n :}
   destination cap SPAN-CHECK
   needed cap > if E-CAPACITY throw then
   source size destination cap OVERLAP? if E-ALIAS throw then
   source size mode destination DECODE-WRITE
   needed ;

: NORMAL-EQUAL? ( ptr u8 n ptr u8 n -- bool )
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
