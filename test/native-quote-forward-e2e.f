\ A typed callback can receive and execute another typed quotation.
require src/lib/test.f

package NATIVE-QUOTE-FORWARD-E2E

private

variable HIT

: INNER ( -- ) 11 HIT ! ;

: OUTER ( [ [ -- ] -- ] -- ) {: callback :}
   [: INNER ;] callback execute ;

: INVOKE ( [ -- ] -- ) execute ;

: RUN ( -- )
   [: INVOKE ;] OUTER ;

: COUNT-INNER ( -- ) 1 HIT +! ;

: TWO-QUOT-RUN ( [ -- ] [ -- ] -- [ -- ] [ -- ] )
   over execute dup execute ;

: WRONG-EFFECT ( -- )
   s" : WRONG ( -- ) [: 1 ;] [: NATIVE-QUOTE-FORWARD-E2E:INVOKE ;] execute ;" evaluate-closed ;

: CATCH-RUN ( -- )
   [: COUNT-INNER ;] [: COUNT-INNER ;] [: TWO-QUOT-RUN ;] catch
   {: rc:n :}
   2drop
   rc 0<> if rc throw then ;

public

EXPORT INVOKE

: TEST ( -- )
   T-RESET
   0 HIT !
   RUN
   HIT @ 11 T=
   0 HIT !
   CATCH-RUN
   HIT @ 2 T=
   s" a callback with the wrong inner effect is rejected" T-LABEL
   [: WRONG-EFFECT ;] 70 TTHROWSQ
   T-REPORT ;

;package

NATIVE-QUOTE-FORWARD-E2E:TEST
