\ cuda-driver-test.f - checked CUDA Driver binding + fail-closed guard regressions.
\
\ Portable: the host leg proves the fail-closed guards (null handle and nonzero
\ CUresult throw E-CUDA) and the off-device OPEN? smoke without touching a real
\ GPU. The typed FFI bindings and convenience helpers (HTOD / DTOH) are checked
\ at load; their device legs are
\ exercised by the tools/ptx launcher device gate on hardware. CU-MEMCPY-DTOD
\ is checked at load and by the GPU buffer device gate.

require lib/test.f
require lib/ptx/cuda-driver.f

package CUDA-DRIVER-TEST

$200 constant PARAM-DIAG-CAP
create PARAM-DIAG PARAM-DIAG-CAP allot

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

: DTOD-CHECKED ( cuda-devptr cuda-devptr len -- rc )
   CUDA:CU-MEMCPY-DTOD ;

: PARAM-TYPES ( -- )
   s" CDTP-N ( cuda-fn idx ptr n len -- rc ) CUDA:CU-PARAM-SET-V" CHECK-CANDIDATE! -1 T=
   s" CDTP-U8 ( cuda-fn idx ptr u8 len -- rc ) CUDA:CU-PARAM-SET-V" CHECK-CANDIDATE! -1 T=
   PARAM-DIAG PARAM-DIAG-CAP DIAG-BUFFER!
   s" CDTP-RAW ( cuda-fn idx n len -- rc ) CUDA:CU-PARAM-SET-V" CHECK-CANDIDATE! 0 T=
   DIAG-BUFFER-OFF ;

: OUTPUT-TYPES ( -- )
   s" CDTO-N ( ptr n idx -- rc ) CUDA:CU-DEVICE-GET" CHECK-CANDIDATE! -1 T=
   PARAM-DIAG PARAM-DIAG-CAP DIAG-BUFFER!
   s" CDTO-U8 ( ptr u8 idx -- rc ) CUDA:CU-DEVICE-GET" CHECK-CANDIDATE! 0 T=
   s" CDTO-RAW ( n idx -- rc ) CUDA:CU-DEVICE-GET" CHECK-CANDIDATE! 0 T=
   DIAG-BUFFER-OFF ;

: RUN ( -- )
   T-RESET
   OPEN-SMOKE
   HANDLE-BAD
   RC-OK
   RC-BAD
   PARAM-TYPES
   OUTPUT-TYPES
   T-REPORT ;

RUN

;package
