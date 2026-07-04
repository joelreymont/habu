\ cuda-driver-test.f - checked CUDA Driver binding + fail-closed guard regressions.
\
\ Portable: the host leg proves the fail-closed guards (null handle and nonzero
\ CUresult throw E-CUDA) and the off-device OPEN? smoke without touching a real
\ GPU. The typed FFI bindings and convenience helpers (LOAD-MODULE / GET-FUNCTION
\ / DEVICE-ALLOC / HTOD / DTOH) are checked at load; their device legs are
\ exercised by the tools/ptx launcher device gate on hardware.

require lib/test.f
require lib/ptx/cuda-driver.f

package CUDA-DRIVER-TEST

: OPEN-SMOKE ( -- )                               \ off-device: no handle; on-device: nonnull
   CUDA:OPEN? if
      CUDA:HANDLE@ CUDA:HANDLE0 drop
   else
      CUDA:HANDLE@ 0 T=
   then ;

: HANDLE-BAD ( -- )                               \ a null out-handle fails closed
   [: 0 CUDA:HANDLE0 drop ;] E-CUDA TTHROWSQ ;

: RC-OK ( -- )                                    \ CUDA_SUCCESS passes
   0 >RC CUDA:RC0 ;

: RC-BAD ( -- )                                   \ any nonzero CUresult fails closed
   [: 7 >RC CUDA:RC0 ;] E-CUDA TTHROWSQ ;

: RUN ( -- )
   T-RESET
   OPEN-SMOKE
   HANDLE-BAD
   RC-OK
   RC-BAD
   T-REPORT ;

RUN

end-package
