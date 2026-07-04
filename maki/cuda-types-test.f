\ cuda-types-test.f - CUDA role helper regressions.
\ Reopens `package CUDA` to white-box the private handle-check helpers by bare name
\ (docs/forth.md "Qualify only across package boundaries").

require lib/test.f
require maki/cuda-types.f

\ maki-ns-lint: boundary CUDA - CUDA subsystem white-box test (reopens package CUDA)
package CUDA

: CUDA-HANDLE-OK ( -- )
   5 CUDA-HANDLE0 5 T= ;

: CUDA-HANDLE-BAD ( -- )
   [: 0 CUDA-HANDLE0 drop ;] E-MK-GPU TTHROWSQ ;

: CUDA-RC-OK ( -- )
   0 >RC CUDA-RC0 ;

: CUDA-RC-BAD ( -- )
   [: 7 >RC CUDA-RC0 ;] E-MK-GPU TTHROWSQ ;

: CUDA-TYPES-TEST ( -- )
   T-RESET
   CUDA-HANDLE-OK
   CUDA-HANDLE-BAD
   CUDA-RC-OK
   CUDA-RC-BAD
   T-REPORT ;

CUDA-TYPES-TEST

end-package
