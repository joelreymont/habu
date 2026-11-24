\ XML 1.0 scalar/entity decoding retains byte offsets in its input view.
require src/lib/prelude.f
require src/lib/utf8-scalar.f
require src/lib/string.f
require src/lib/vector.f
require src/lib/xml/reject.f

package XML
public
\ XML scalar/parser errors: -9201..-9208, -9212..-9214, -9216 and -9217.
-9201 s" an output would pass the largest size"
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

private
$7FFFFFFFFFFFFFFF constant MAX-SIZE
0 constant DECODE-TEXT
1 constant DECODE-ATTR
2 constant DECODE-RAW

\ A numeric address view is needed solely for extent checks.
: BYTE-ADDRESS ( ^u8 -- n ) NULL-PTR BYTE-VIEW - ;

: SPAN-CHECK ( str -- )
   {: source:str :}
   source STR:LEN 0 < if E-RANGE throw then
   source STR:DATA BYTE-ADDRESS {: start:n :}
   start 0 < if E-RANGE throw then
   source STR:LEN 0 > start 0= and if E-RANGE throw then
   source STR:LEN MAX-SIZE start - > if E-RANGE throw then ;

\ Each helper below rejects at the failing byte of the span it is given, an entity
\ reference at its `&`, counted from at, the place of the span's first byte.
: BYTE@ ( str place n -- n )
   {: source:str at:place index:n :}
   index 0 < index source STR:LEN >= or if
      source at index 0 max source STR:LEN min
      s" the rest of the document"
      s" write the rest of the document" E-TRUNCATED REJECT-FROM
   then
   source STR:DATA index + c@ ;

: BYTES= ( str str -- bool )
   {: first:str second:str :}
   first STR:LEN second STR:LEN <> if false exit then
   first STR:LEN 0 ?do
      first STR:DATA i + c@ second STR:DATA i + c@ <> if false unloop exit then
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

: SCALAR-EXPECTED$ ( -- str ) s" a character XML 1.0 allows" ;
: SCALAR-FIX$ ( -- str )
   s" remove the character, or write one XML 1.0 allows" ;

\ Rejects the scalar at off in the span unless XML 1.0 allows it.
: REQUIRE-SCALAR ( str place n n -- )
   {: source:str at:place off:n scalar:n :}
   scalar XML-SCALAR? 0= if
      source at off SCALAR-EXPECTED$ SCALAR-FIX$ E-SCALAR
      REJECT-FROM
   then ;

\ The offset of the first byte above $7F, or n when there is none.
: HIGH-BYTE ( str -- n )
   {: source:str :}
   source STR:LEN 0 ?do source STR:DATA i + c@ $7F > if i unloop exit then loop
   source STR:LEN ;

: ASCII-EXPECTED$ ( -- str ) s" a US-ASCII character" ;
: ASCII-FIX$ ( -- str )
   s\" write it as a character reference such as `&#233;`, or save the document as UTF-8 and declare `encoding=\qUTF-8\q`" ;

: ASCII-WIRE ( str -- )
   {: source:str :}
   source HIGH-BYTE {: off:n :}
   off source STR:LEN < if
      source WHOLE off ASCII-EXPECTED$ ASCII-FIX$ E-ENCODING
      REJECT-FROM
   then ;

: SCALAR-AT ( str place n -- n n )
   {: source:str at:place cursor:n :}
   source at cursor BYTE@ drop
   source cursor
   UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF ENDOF
      raw-byte OF
         2drop source at cursor s" a UTF-8 character"
         s" write the text in UTF-8" E-UTF8 REJECT-FROM
      ENDOF
   ;MATCH
   {: scalar:n next:n :}
   source at cursor scalar REQUIRE-SCALAR
   scalar next ;

: DIGIT-REJECT ( str place n -- )
   {: source:str at:place amp:n :}
   source at amp
   s" a character reference such as `&#65;` or `&#x41;`"
   s" write the character's number in decimal after `&#`, or in hex after `&#x`, and end the reference with `;`"
   E-ENTITY REJECT-FROM ;

: NAME-REJECT ( str place n -- )
   {: source:str at:place amp:n :}
   source at amp
   s" `&amp;`, `&lt;`, `&gt;`, `&quot;`, `&apos;` or a character reference such as `&#65;`"
   s" write `&amp;` for a literal `&`, and end each reference with `;`"
   E-ENTITY REJECT-FROM ;

\ The value of a digit of the reference at amp.
: HEX-DIGIT ( str place n n -- n )
   {: source:str at:place amp:n byte:n :}
   byte STR:HEX-DIGIT? 0= if source at amp DIGIT-REJECT then
   byte STR:HEX-DIGIT-VALUE ;

\ The value of the reference at amp after one more digit.
: NUMERIC-STEP ( str place n n n n -- n )
   {: source:str at:place amp:n value:n digit:n radix:n :}
   digit radix >= if source at amp DIGIT-REJECT then
   value $10FFFF digit - radix / > if
      source at amp s" a character number at most `&#x10FFFF;`"
      s" write the number of a Unicode character" E-ENTITY REJECT-FROM
   then
   value radix * digit + ;

\ The scalar of the character reference at amp and the offset after its `;`.
: NUMERIC-ENTITY ( str place n -- n n )
   {: source:str at:place amp:n :}
   source at amp 2 + BYTE@ $78 = if
      amp 3 + $10
   else
      amp 2 + 10
   then
   {: digits:n radix:n :}
   0 digits
   begin
      {: value:n cursor:n :}
      source at cursor BYTE@ $3B = if
         cursor digits = if source at amp DIGIT-REJECT then
         source at amp value REQUIRE-SCALAR
         value cursor 1+ exit
      then
      source at amp source at cursor BYTE@ HEX-DIGIT
      {: digit:n :}
      source at amp value digit radix NUMERIC-STEP
      cursor 1+
   again ;

\ The scalar a predefined entity names, given the reference at amp and its `;`
\ at end.
: NAMED-SCALAR ( str place n n -- n )
   {: source:str at:place amp:n end:n :}
   source STR:DATA amp 1+ + end amp - 1- >str {: name:str :}
   name s" lt" BYTES= if $3C exit then
   name s" gt" BYTES= if $3E exit then
   name s" amp" BYTES= if $26 exit then
   name s" apos" BYTES= if $27 exit then
   name s" quot" BYTES= if $22 exit then
   source at amp NAME-REJECT ;

: SEMICOLON? ( n str place -- bool )
   {: cursor:n source:str at:place :}
   source at cursor BYTE@ $3B = ;

: ENTITY-AT ( str place n -- n n )
   {: source:str at:place start:n :}
   source at start 1+ BYTE@ $23 = if
      source at start NUMERIC-ENTITY exit
   then
   start 1+
   begin
      dup source at SEMICOLON? 0=
   while
      dup start - 5 > if source at start NAME-REJECT then
      1+
   repeat
   {: end:n :}
   source at start end NAMED-SCALAR
   end 1+ ;

: NORMALIZED ( str n n n -- n n )
   {: source:str mode:n scalar:n next:n :}
   scalar $D = if
      next source STR:LEN < if
         source STR:DATA next + c@ $A = if next 1+ else next then
      else next then
      $A swap
   else scalar next then
   {: value:n after:n :}
   mode DECODE-ATTR = value XML-SPACE? and if
      $20 after
   else value after then ;

: DECODE-AT ( str place n n -- n n )
   {: source:str at:place cursor:n mode:n :}
   mode DECODE-RAW <> if
      source at cursor BYTE@ $26 = if
         source at cursor ENTITY-AT exit
      then
   then
   source mode source at cursor SCALAR-AT NORMALIZED ;

: SCALAR-WIDTH ( n -- n )
   dup $80 < if drop 1 exit then
   dup $800 < if drop 2 exit then
   $10000 < if 3 else 4 then ;

: DECODED-SIZE ( str n -- n )
   {: source:str mode:n :}
   source SPAN-CHECK
   0 0
   begin dup source STR:LEN < while
      {: total:n cursor:n :}
      source WHOLE cursor mode DECODE-AT
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

: DECODE-WRITE ( str n ^u8 -- )
   {: source:str mode:n destination :}
   destination 0
   begin dup source STR:LEN < while
      {: next-output cursor:n :}
      source WHOLE cursor mode DECODE-AT
      {: scalar:n next:n :}
      scalar next-output PUT-SCALAR next
   repeat
   2drop ;

\ A reader's value was checked when NEXT read it, so it rejects nothing here.
: DECODE-INTO ( str n ^VEC:vector<u8> -- )
   {: source:str mode:n v:^ :}
   source mode DECODED-SIZE {: needed:n :}
   v VEC:COUNT {: at:n :}
   v at needed + VEC:ENSURE
   v VEC:SPAN SPAN:$ STR:DATA at + {: out:^ :}
   source mode out DECODE-WRITE ;

: NORMAL-EQUAL? ( str str -- bool )
   {: first:str second:str :}
   0 0
   begin
      {: first-index:n second-index:n :}
      first-index first STR:LEN = second-index second STR:LEN = or if
         first-index first STR:LEN = second-index second STR:LEN = and exit
      then
      first WHOLE first-index DECODE-ATTR DECODE-AT
      {: first-scalar:n first-next:n :}
      second WHOLE second-index DECODE-ATTR
      DECODE-AT
      {: second-scalar:n second-next:n :}
      first-scalar second-scalar <> if false exit then
      first-next second-next
   again ;

;package
