\ checker-soundness-suite.f - a certified body must not exceed its declared effect.
\ Each case pins one way a body used to launder a value past its contract.

require lib/test.f
require test/checker-assert.f

package CHECKER-SOUNDNESS
public

TRUSTED: TAKE-U8 ( u8 -- ) drop ;
TRUSTED: TAKE-I64 ( i64 -- ) drop ;
TRUSTED: TAKE-RU8 ( | u8 -- ) r> drop ;
TRUSTED: TAKE-RI64 ( | i64 -- ) r> drop ;
TRUSTED: MK-I64 ( -- i64 ) 300 ;
TRUSTED: MK-U8 ( -- u8 ) 3 ;
: CALL-I64 ( [ i64 -- ] -- ) MK-I64 swap execute ;
: CALL-U8 ( [ u8 -- ] -- ) MK-U8 swap execute ;
: CALL-RI64 ( [ -- | i64 -- ] -- ) MK-I64 >r execute ;
: CALL-RU8 ( [ -- | u8 -- ] -- ) MK-U8 >r execute ;
: CALL-I64-Q ( [ [ i64 -- ] -- ] -- ) ['] TAKE-I64 swap execute ;
: CALL-U8-Q ( [ [ u8 -- ] -- ] -- ) ['] TAKE-U8 swap execute ;

\ CHECK-QUIET-CANDIDATE!: -1 certified, 0 rejected, 1 uncheckable.
: QUOTATION-INPUTS ( -- )
   s" a quotation consuming a narrower input does not satisfy a wider parameter" T-LABEL
   s" CS-Q1 ( -- ) [: CHECKER-SOUNDNESS:TAKE-U8 ;] CHECKER-SOUNDNESS:CALL-I64"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" a quotation consuming a wider input satisfies a narrower parameter" T-LABEL
   s" CS-Q2 ( -- ) [: CHECKER-SOUNDNESS:TAKE-I64 ;] CHECKER-SOUNDNESS:CALL-U8"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" an exact quotation input still passes" T-LABEL
   s" CS-Q3 ( -- ) [: CHECKER-SOUNDNESS:TAKE-I64 ;] CHECKER-SOUNDNESS:CALL-I64"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" return-row inputs read the same direction" T-LABEL
   s" CS-Q4 ( -- ) [: CHECKER-SOUNDNESS:TAKE-RU8 ;] CHECKER-SOUNDNESS:CALL-RI64"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" CS-Q5 ( -- ) [: CHECKER-SOUNDNESS:TAKE-RI64 ;] CHECKER-SOUNDNESS:CALL-RU8"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" a nested quotation input reverses the direction twice" T-LABEL
   s" CS-Q6 ( -- ) [: CHECKER-SOUNDNESS:CALL-U8 ;] CHECKER-SOUNDNESS:CALL-I64-Q"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" CS-Q7 ( -- ) [: CHECKER-SOUNDNESS:CALL-I64 ;] CHECKER-SOUNDNESS:CALL-U8-Q"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" the accepted shapes execute" T-LABEL
   [: TAKE-I64 ;] CALL-U8
   [: TAKE-RI64 ;] CALL-RU8
   [: CALL-U8 ;] CALL-I64-Q
   depth 0 T= ;

: RUN ( -- )
   T-RESET
   QUOTATION-INPUTS
   T-REPORT ;

RUN
;package
