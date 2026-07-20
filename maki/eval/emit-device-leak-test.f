\ maki/eval/emit-device-leak-test.f - HOST-ONLY leak proof for the migrated EVND runner.
\
\ maki/eval/emit-device.f runs each device golden (RUN-SUMNORM/GEMM/ATTN) under a
\ SINGLE CUDA-SCOPE frame that owns the primary context, the module, AND every
\ device buffer, so a throw anywhere in the pipeline unwinds all of them in reverse
\ - the exact "a throw leaks every allocation, module, and retained primary context"
\ case the dot names. This drives the REAL RUN-SUMNORM-CORE off-device through the
\ shared recording fake (maki/cuda-run-fake.f) and proves, both directions, that the
\ pre-migration sequence (no ownership transfer) releases nothing on a failure while
\ the migrated runner releases exactly the owned prefix, reversed, exactly once.

require lib/test.f
require maki/eval/emit-device.f
require maki/cuda-run-fake.f

package EVND                         \ reopen for RUN-SUMNORM-CORE + the SN-* / EVND-* globals

\ pre-migration RUN-SUMNORM acquire+alloc leg (no ownership transfer): a failure leaks
: ELK-BASE-SN ( -- )
   MKD:OPEN  0 MKD:CUINIT CUDA:RC0
   EVND-DEV 0 >IDX MKD:CUDEVICEGET CUDA:RC0
   EVND-CTX EVND-DEV @ >CUDA-DEV MKD:CUDEVICEPRIMARYCTXRETAIN CUDA:RC0
   EVND-CTX @ >CUDA-CTX MKD:CUCTXSETCURRENT CUDA:RC0
   MAKI-GRADE:CUBIN$ EVND-PATH >CSTR
   EVND-MOD EVND-PATH MKD:CUMODULELOAD CUDA:RC0
   s" SOFTMAX_ROWS" EVND-KN >CSTR
   EVND-FUNC EVND-MOD @ >CUDA-MOD EVND-KN MKD:CUMODULEGETFUNCTION CUDA:RC0
   SN-DIN SN-BYTES >LEN MKD:CUMEMALLOC CUDA:RC0
   SN-DOUT SN-BYTES >LEN MKD:CUMEMALLOC CUDA:RC0
   SN-DIN @ >CUDA-DEVPTR SN-IN SN-BYTES >LEN MKD:CUMEMCPYHTOD CUDA:RC0 ;

: ELK-SN-SCOPED ( -- )  [: RUN-SUMNORM-CORE ;] CUDA-SCOPE:SCOPE ;   \ the migrated per-run boundary

: ELK-T-GETFUNC ( -- )               \ getfunction fails: unwind module + primary context
   MKDF:RESET  7 MKDF:FUNC-RC !
   [: ELK-BASE-SN ;] E-CUDA TTHROWSQ  MKDF:RN@ 0 T=
   MKDF:RESET  7 MKDF:FUNC-RC !
   [: ELK-SN-SCOPED ;] E-CUDA TTHROWSQ
   MKDF:RN@ 2 T=  0 MKDF:REL@ MKDF:MOD-H T=  1 MKDF:REL@ MKDF:DEV-H T=
   CUDA-SCOPE:DEPTH 0 T= ;

: ELK-T-ALLOC2 ( -- )                \ second alloc fails: unwind alloc0 + module + ctx
   MKDF:RESET  1 MKDF:FAIL-A !  5 MKDF:A-RC !
   [: ELK-BASE-SN ;] E-CUDA TTHROWSQ  MKDF:RN@ 0 T=
   MKDF:RESET  1 MKDF:FAIL-A !  5 MKDF:A-RC !
   [: ELK-SN-SCOPED ;] E-CUDA TTHROWSQ
   MKDF:RN@ 3 T=
   0 MKDF:REL@ 0 MKDF:ALLOC-H T=  1 MKDF:REL@ MKDF:MOD-H T=  2 MKDF:REL@ MKDF:DEV-H T=
   CUDA-SCOPE:DEPTH 0 T= ;

: ELK-T-HTOD ( -- )                  \ mid-pipeline copy throws: unwind BOTH allocs + module + ctx, reversed
   MKDF:RESET  9 MKDF:HTOD-RC !
   [: ELK-BASE-SN ;] E-CUDA TTHROWSQ  MKDF:RN@ 0 T=
   MKDF:RESET  9 MKDF:HTOD-RC !
   [: ELK-SN-SCOPED ;] E-CUDA TTHROWSQ
   MKDF:RN@ 4 T=
   0 MKDF:REL@ 1 MKDF:ALLOC-H T=  1 MKDF:REL@ 0 MKDF:ALLOC-H T=
   2 MKDF:REL@ MKDF:MOD-H T=  3 MKDF:REL@ MKDF:DEV-H T=
   CUDA-SCOPE:DEPTH 0 T= ;

: ELK-RUN ( -- )
   T-RESET
   s" habu-evnd-leak-test" MAKI-GRADE:PREPARE            \ so CU-LOAD's CUBIN$ path resolves (module load faked)
   MKDF:ON
   ELK-T-GETFUNC
   ELK-T-ALLOC2
   ELK-T-HTOD
   MKDF:OFF
   MAKI-GRADE:CLEAN
   T-REPORT ;

ELK-RUN

;package
s" emit-device-leak-test: ok" type cr
