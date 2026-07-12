\ cuda-driver-test.f - shared CUDA Driver binding regressions.

require lib/test.f
require maki/cuda-driver.f

package CUDA-TEST

: OPEN-SMOKE ( -- )
   CUDA:OPEN? if
      CUDA:HANDLE@ CUDA:HANDLE0 drop
   else
      CUDA:HANDLE@ 0 T=
   then ;

: HANDLE-BAD ( -- )
   [: 0 CUDA:HANDLE0 drop ;] E-MK-GPU TTHROWSQ ;

: RC-OK ( -- )
   0 >RC CUDA:RC0 ;

: RC-BAD ( -- )
   [: 7 >RC CUDA:RC0 ;] E-MK-GPU TTHROWSQ ;

: RUN ( -- )
   T-RESET
   OPEN-SMOKE
   HANDLE-BAD
   RC-OK
   RC-BAD
   T-REPORT ;

RUN

;package
