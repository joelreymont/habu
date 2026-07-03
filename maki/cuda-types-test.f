\ cuda-types-test.f - CUDA role helper regressions.

require lib/test.f
require maki/cuda-types.f

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
