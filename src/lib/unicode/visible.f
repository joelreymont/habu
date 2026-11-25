\ visible.f - text with each Cc, Cf, Zl and Zp character written `U+` and at
\ least four uppercase hex digits, and each byte no UTF-8 character starts
\ written `\x` and two hex digits.
\
\ STORAGE CLASS. PROCESS-WIDE read-only table (src/lib/unicode/visible-data.f,
\ generated); any task may call.

require src/lib/span.f
require src/lib/vector.f
require src/lib/sort.f
require src/lib/utf8-scalar.f
require src/lib/unicode/visible-data.f

package UNICODE-VISIBLE

private

: ENTRY ( ^n n -- ^n )
   {: table:^ idx:n :}
   table idx 2 * cells + ;

public

\ Unicode General_Category Cc, Cf, Zl or Zp; false outside the scalars.
: HIDDEN? ( n -- bool ) {: cp:n :}
   HIDDEN-RANGES {: ranges:^ count:n :}
   0 count [: ranges swap ENTRY @ cp <= ;] SORT:PARTITION-POINT {: at:n :}
   at 0= if false exit then
   cp ranges at 1- ENTRY cell+ @ <= ;

private

: DIGITS ( n -- n ) {: cp:n :}
   cp $FFFF <= if 4 exit then
   cp $FFFFF <= if 5 exit then
   6 ;

\ Writes the low count hex digits of n, highest first.
: HEX-PUT ( ^u8 n n -- ^u8 ) {: dst:^ v:n count:n :}
   count 0 ?do
      s" 0123456789ABCDEF" STR:DATA v count 1- i - 4 * rshift $F and + c@
      dst i + c!
   loop
   dst count + ;

\ The bytes written for the character at the cursor, and the cursor after it.
: STEP-LEN ( str n -- n n ) {: s:str at:n :}
   s at UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF {: cp:n next:n :}
         cp HIDDEN? if cp DIGITS 2 + else next at - then next
      ENDOF
      raw-byte OF nip 4 swap ENDOF
   ;MATCH ;

: PUT-BYTES ( ^u8 str -- ^u8 ) {: dst:^ src:str :}
   src STR:DATA dst src STR:LEN BYTE-COPY dst src STR:LEN + ;

\ Writes the character at the cursor; answers the address after it and the
\ cursor after the character.
: STEP-PUT ( ^u8 str n -- ^u8 n ) {: dst:^ s:str at:n :}
   s at UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF {: cp:n next:n :}
         cp HIDDEN? if
            dst s" U+" PUT-BYTES cp cp DIGITS HEX-PUT
         else
            dst s STR:DATA at + next at - >str PUT-BYTES
         then next
      ENDOF
      raw-byte OF {: b:n next:n :}
         dst s" \x" PUT-BYTES b 2 HEX-PUT next
      ENDOF
   ;MATCH ;

public

\ The bytes PUT writes for the text.
: LEN ( str -- n ) {: s:str :}
   0 0 begin dup s STR:LEN < while s swap STEP-LEN -rot + swap repeat drop ;

\ Writes the text at the address; answers the address after it.
: PUT ( ^u8 str -- ^u8 ) {: dst:^ s:str :}
   dst 0 begin dup s STR:LEN < while s swap STEP-PUT repeat drop ;

\ Writes the text's visible form at the vector's end; the text may not lie in
\ the vector, whose growth releases it.
: APPEND ( str ^VEC:vector<u8> -- ) {: s:str v:^ :}
   v VEC:COUNT {: at:n :}
   v at s LEN + VEC:ENSURE
   v VEC:SPAN at SPAN:SKIP SPAN:$ STR:DATA s PUT drop ;

;package
