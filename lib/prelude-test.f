\ prelude-test.f - coverage for the prelude conveniences.
\ Load after lib/errors.f lib/string.f lib/test.f lib/float.f lib/prelude.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/float.f
require lib/prelude.f
require lib/ieee754.f

package PRELUDE-TEST

: TEST-UNORDERED ( r -- ) {: nan:r :}
   nan 1.0 f<= TFALSE nan 1.0 f>= TFALSE
   1.0 nan f<= TFALSE 1.0 nan f>= TFALSE
   nan nan f<= TFALSE nan nan f>= TFALSE ;

: TEST-SPECIAL ( -- )
   $7FF8000000000000 IEEE754:BITS>F64 TEST-UNORDERED
   $7FF0000000000001 IEEE754:BITS>F64 TEST-UNORDERED
   $FFF8000000000000 IEEE754:BITS>F64 TEST-UNORDERED
   $8000000000000000 IEEE754:BITS>F64 {: negzero:r :}
   0.0 negzero f<= TTRUE negzero 0.0 f<= TTRUE
   0.0 negzero f>= TTRUE negzero 0.0 f>= TTRUE
   $7FF0000000000000 IEEE754:BITS>F64 {: inf:r :}
   $FFF0000000000000 IEEE754:BITS>F64 {: neginf:r :}
   inf inf f<= TTRUE inf inf f>= TTRUE
   neginf neginf f<= TTRUE neginf neginf f>= TTRUE
   neginf inf f<= TTRUE inf neginf f>= TTRUE
   inf neginf f<= TFALSE neginf inf f>= TFALSE
   1.0 inf f<= TTRUE inf 1.0 f<= TFALSE
   1.0 neginf f>= TTRUE neginf 1.0 f>= TFALSE ;

: PRE-RUN ( -- )
   T-RESET
   TEST-SPECIAL
   true  TTRUE
   false TFALSE
   5 0<> TTRUE
   0 0<> TFALSE
   7.0 fdrop                     \ fdrop leaves a clean stack
   7.0 fdup f= TTRUE             \ fdup duplicates the top float
   3.0 9.0 fover f> TTRUE fdrop  \ fover copies 2nd to top: 3.0 9.0 3.0
   3.0 5.0 f<= TTRUE   5.0 3.0 f<= TFALSE
   3.0 3.0 f<= TTRUE   3.0 3.0 f>= TTRUE
   5.0 3.0 f>= TTRUE   3.0 5.0 f>= TFALSE ;

PRE-RUN
T-REPORT

;package
