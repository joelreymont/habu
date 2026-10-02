\ A typed callback can receive and execute another typed quotation.
require lib/test.f
require lib/fs.f
require lib/fs-mutate.f

1 set-tier

package NATIVE-QUOTE-FORWARD-E2E

private

variable HIT
create RESULT 1 allot
create PATH FS-PATH-CAP allot

: INNER ( -- ) 11 HIT ! ;

TRUSTED: OUTER ( [ [ -- ] -- ] -- ) {: callback :}
   [: INNER ;] callback execute ;

: INVOKE ( [ -- ] -- ) execute ;

: RUN ( -- )
   [: INVOKE ;] OUTER ;

: COUNT-INNER ( -- ) 1 HIT +! ;

: TWO-QUOT-RUN ( [ -- ] [ -- ] -- [ -- ] [ -- ] )
   over execute dup execute ;

TRUSTED: WRONG-EFFECT ( -- )
   s" : WRONG ( -- ) [: 1 ;] [: NATIVE-QUOTE-FORWARD-E2E:INVOKE ;] execute ;" evaluate ;

TRUSTED: CATCH-RUN ( -- )
   [: COUNT-INNER ;] [: COUNT-INNER ;] [: TWO-QUOT-RUN ;] catch
   {: rc:n :}
   2drop
   rc 0<> if rc throw then ;

public

EXPORT INVOKE

TRUSTED: TEST ( -- )
   T-RESET
   0 HIT !
   RUN
   HIT @ 11 T=
   0 HIT !
   CATCH-RUN
   HIT @ 2 T=
   s" a callback with the wrong inner effect is rejected" T-LABEL
   [: WRONG-EFFECT ;] 70 TTHROWSQ
   T-REPORT
   s" native-quote-forward" HB-TMP-MKDIR {: dir:ptr len:n :}
   dir len s" result.bin" PATH JOIN-PATH {: path-u:n :}
   HIT @ RESULT c!
   PATH path-u RESULT 1 WRITE-ALL
   s" native quote artifact: " type PATH path-u type cr ;

;package

NATIVE-QUOTE-FORWARD-E2E:TEST
