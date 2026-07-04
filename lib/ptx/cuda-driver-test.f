\ cuda-driver-test.f - focused tests for fail-closed CUDA Driver wrappers.

require lib/test.f
require lib/ptx/cuda-driver.f

package CUDA-TEST

: RC-FAILS ( -- )
   7 CUDA:CHECK-RC ;

: LIB-FAILS ( -- )
   s" /definitely/not/a/cuda/driver.so" CUDA:OPEN-LIB ;

: SYM-FAILS ( -- )
   0 CUDA:CHECK-SYM drop ;

: GOLDEN-FAILS ( -- )
   $40A00000 $40C00000 CUDA:EXPECT-GOLDEN ;

: RUN ( -- )
   T-RESET
   CUDA:RESET
   0 CUDA:CHECK-RC
   CUDA:LAST-RC@ 0 T=
   1 CUDA:CHECK-LIB drop
   1 CUDA:CHECK-SYM drop
   $40C00000 $40C00000 CUDA:EXPECT-GOLDEN
   [: RC-FAILS ;] E-PTX-CUDA-RC TTHROWSQ
   CUDA:LAST-RC@ 7 T=
   [: LIB-FAILS ;] E-PTX-CUDA-DLOPEN TTHROWSQ
   [: SYM-FAILS ;] E-PTX-CUDA-DLSYM TTHROWSQ
   [: GOLDEN-FAILS ;] E-PTX-DEVICE-GOLDEN TTHROWSQ
   T-REPORT ;

RUN

end-package
