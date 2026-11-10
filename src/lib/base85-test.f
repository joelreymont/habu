\ base85-test.f - Adobe Ascii85: whole groups, every tail length, `z`, each
\ white-space byte, the largest group and every refusal.
\ Run: bin/hb --load src/lib/base85-test.f

require src/lib/string.f
require src/lib/test.f
require src/lib/span.f
require src/lib/base85.f

package BASE85-TEST

$A5 constant CANARY

64 SPAN-BUFFER: OUT
16 SPAN-BUFFER: WANT
16 SPAN-BUFFER: STAGED
3 SPAN-BUFFER: OUT-3
variable STAGED-LEN

: OUT$ ( n -- ptr u8 n )
   OUT SPAN:$ drop swap ;


: DECODES ( ptr u8 n ptr u8 n -- )
   {: a u:n want wu:n :}
   a u T-LABEL
   a u OUT BASE85:DECODE {: k:n :}
   k OUT$ want wu T$= ;


\ k bytes of value b, then the text t: what WANT holds after it.
: BYTES ( n n ptr u8 n -- ptr u8 n )
   {: b:n k:n t tu:n :}
   k 0 ?do b WANT i SPAN:U8! loop
   tu 0 ?do t i + c@ WANT k i + SPAN:U8! loop
   WANT SPAN:$ drop k tu + ;


\ A refusal runs in a quotation, which sees no locals, so its input is staged.
: STAGE ( ptr u8 n -- )
   {: a u:n :}
   a u STAGED SPAN:COPY
   u STAGED-LEN ! ;


: STAGED$ ( -- ptr u8 n )
   STAGED SPAN:$ drop STAGED-LEN @ ;


: REFUSES ( ptr u8 n n -- )
   {: a u:n code:n :}
   a u STAGE
   a u T-LABEL
   [: STAGED$ OUT BASE85:DECODE drop ;] code TTHROWSQ ;


: VECTORS ( -- )
   s" 9jqo^" s" Man " DECODES
   s" BOu!rDZ" s" hello" DECODES
   s" @/" s" a" DECODES
   s" @:B" s" ab" DECODES
   s" @:E^" s" abc" DECODES
   s" 9jqo^BlbD-BleB1DJ+*+F(f,q" s" Man is distinguished" DECODES
   0 OUT$ 0 OUT$ DECODES ;


: ZEROS ( -- )
   s" z" 0 4 0 OUT$ BYTES DECODES
   s" z@:B" 0 4 s" ab" BYTES DECODES
   s" s8W-!" $FF 4 0 OUT$ BYTES DECODES ;


\ White space between groups, then all six white-space bytes around one group:
\ space before it, tab, LF, CR and FF between its digits, NUL after it.
: SPACES ( -- )
   s" Man ....ab" WANT SPAN:COPY
   4 0 do 0 WANT i 4 + SPAN:U8! loop
   s" 9jqo^ z.@:E" STAGE $0A STAGED 7 SPAN:U8!
   STAGED$ WANT SPAN:$ drop 10 DECODES
   s"  9.j.q.o.^." STAGE
   $09 STAGED 2 SPAN:U8!  $0A STAGED 4 SPAN:U8!  $0D STAGED 6 SPAN:U8!
   $0C STAGED 8 SPAN:U8!  $00 STAGED 10 SPAN:U8!
   STAGED$ s" Man " DECODES ;


: REFUSALS ( -- )
   s" s8W-!" STAGE $22 STAGED 4 SPAN:U8!
   STAGED$ BASE85:E-VALUE REFUSES
   s" s8W-" BASE85:E-VALUE REFUSES
   s" 9jzqo" BASE85:E-Z REFUSES
   s" 9jqov" BASE85:E-CHAR REFUSES
   CANARY OUT SPAN:FILL
   s" 9jqo^B" BASE85:E-LENGTH REFUSES
   s" a refusal after a good group leaves the output as it was" T-LABEL
   OUT 0 SPAN:U8@ CANARY T=
   CANARY OUT-3 SPAN:FILL
   s" a span one byte short" T-LABEL
   [: s" 9jqo^" OUT-3 BASE85:DECODE drop ;] E-SPAN-CAPACITY TTHROWSQ
   s" and writes none of it" T-LABEL
   OUT-3 0 SPAN:U8@ CANARY T=
   s" a span of exactly the decoded size" T-LABEL
   s" @:E^" OUT-3 BASE85:DECODE 3 T=
   s" a negative count" T-LABEL
   [: 0 OUT$ drop -1 OUT BASE85:DECODE drop ;] E-SPAN-LENGTH TTHROWSQ ;

public

: RUN ( -- )
   T-RESET
   VECTORS ZEROS SPACES REFUSALS ;

;package

BASE85-TEST:RUN
T-REPORT
