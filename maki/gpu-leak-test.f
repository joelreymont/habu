\ maki/gpu-leak-test.f - HOST-ONLY leak proof for the migrated GPU CUDA lifecycle.
\
\ Drives the REAL migrated maki/gpu.f words (SETUP-OPEN's atomic acquire and
\ LAUNCH-CORE's per-launch scope) off-device: every driver call the runner makes
\ is re-pointed at the shared recording fake (maki/cuda-run-fake.f) and CUDA-SCOPE's
\ release defers count what is freed. Each leak class the dot names is proved BOTH
\ directions in one file - the pre-migration acquire sequence (no ownership
\ transfer) releases NOTHING on the failure, while the migrated word releases
\ exactly the owned prefix, in reverse acquisition order, exactly once:
\   class 1  second cuMemAlloc fails  -> base leaks the first alloc; migrated frees it.
\   class 2  mid-pipeline htod throws -> base leaks both allocs; migrated frees both, reversed.
\   class 3  cuModuleGetFunction fails -> base leaks module+ctx; migrated frees module then ctx.
\ No GPU or libcuda is touched, so it runs in the gate on any host.

require lib/test.f
require maki/gpu.f
require maki/cuda-run-fake.f

package GPU                          \ reopen for SETUP-OPEN / SETUP-CU / LAUNCH-CORE + the G* globals

\ ---- pre-migration reference sequences (acquire with NO ownership transfer) ----
: GLK-BASE-ACQUIRE ( -- )            \ the old SETUP-CU: any failure leaks the owned prefix
   MKD:OPEN
   0 MKD:CUINIT CUDA:RC0
   GDEV 0 >IDX MKD:CUDEVICEGET CUDA:RC0
   GCTX GDEV @ >CUDA-DEV MKD:CUDEVICEPRIMARYCTXRETAIN CUDA:RC0
   GCTX @ >CUDA-CTX MKD:CUCTXSETCURRENT CUDA:RC0
   PTXTC:CUBIN$ GPATH >CSTR
   GMOD GPATH MKD:CUMODULELOAD CUDA:RC0
   s" SAXPY" GKN >CSTR
   GFUNC GMOD @ >CUDA-MOD GKN MKD:CUMODULEGETFUNCTION CUDA:RC0 ;
: GLK-BASE-ALLOC ( -- )              \ the old LAUNCH alloc leg: a failure leaks earlier allocs
   GN 4 *  {: bytes :}
   GDX bytes >LEN MKD:CUMEMALLOC CUDA:RC0
   GDY bytes >LEN MKD:CUMEMALLOC CUDA:RC0
   GDX @ >CUDA-DEVPTR GHX bytes >LEN MKD:CUMEMCPYHTOD CUDA:RC0 ;

: GLK-LAUNCH-SCOPED ( -- )  [: LAUNCH-CORE ;] CUDA-SCOPE:SCOPE ;   \ the migrated per-launch boundary

\ ---- cases -----------------------------------------------------------------
: GLK-T-ALLOC2 ( -- )                \ class 1: the second cuMemAlloc fails
   MKDF:RESET  1 MKDF:FAIL-A !  5 MKDF:A-RC !
   [: GLK-BASE-ALLOC ;] E-CUDA TTHROWSQ
   MKDF:RN@ 0 T=                                         \ base: first alloc leaked (0 released)
   MKDF:RESET  1 MKDF:FAIL-A !  5 MKDF:A-RC !
   [: GLK-LAUNCH-SCOPED ;] E-CUDA TTHROWSQ
   MKDF:RN@ 1 T=  0 MKDF:REL@ 0 MKDF:ALLOC-H T=          \ migrated: exactly the first alloc freed
   CUDA-SCOPE:DEPTH 0 T= ;

: GLK-T-HTOD ( -- )                  \ class 2: a mid-pipeline copy throws after both allocs
   MKDF:RESET  9 MKDF:HTOD-RC !
   [: GLK-BASE-ALLOC ;] E-CUDA TTHROWSQ
   MKDF:RN@ 0 T=                                         \ base: both allocs leaked
   MKDF:RESET  9 MKDF:HTOD-RC !
   [: GLK-LAUNCH-SCOPED ;] E-CUDA TTHROWSQ
   MKDF:RN@ 2 T=  0 MKDF:REL@ 1 MKDF:ALLOC-H T=  1 MKDF:REL@ 0 MKDF:ALLOC-H T=  \ both freed, reversed
   CUDA-SCOPE:DEPTH 0 T= ;

: GLK-T-GETFUNC ( -- )               \ class 3: cuModuleGetFunction fails after ctx+module owned
   MKDF:RESET  7 MKDF:FUNC-RC !
   [: GLK-BASE-ACQUIRE ;] E-CUDA TTHROWSQ
   MKDF:RN@ 0 T=                                         \ base: module + context leaked
   MKDF:RESET  7 MKDF:FUNC-RC !
   [: SETUP-OPEN ;] E-CUDA TTHROWSQ
   MKDF:RN@ 2 T=  0 MKDF:REL@ MKDF:MOD-H T=  1 MKDF:REL@ MKDF:DEV-H T=  \ module then primary-ctx(device)
   CUDA-SCOPE:DEPTH 0 T= ;

: GLK-T-REPEAT ( -- )                \ a clean acquire after the injected failures still works + releases
   MKDF:RESET
   [: SETUP-OPEN ;] catch 0 T=                           \ unarmed -> succeeds
   CUDA-SCOPE:DEPTH 2 T=                                 \ ctx + module owned
   CUDA-SCOPE:UNWIND                                     \ the RELEASE path
   MKDF:RN@ 2 T=  0 MKDF:REL@ MKDF:MOD-H T=  1 MKDF:REL@ MKDF:DEV-H T=  \ reverse: module then primary-ctx
   CUDA-SCOPE:DEPTH 0 T= ;

: GLK-RUN ( -- )
   T-RESET
   s" habu-gpu-leak-test" PTXTC:PREPARE                  \ so SETUP-CU's CUBIN$ path resolves (module load faked)
   MKDF:ON
   GLK-T-ALLOC2
   GLK-T-HTOD
   GLK-T-GETFUNC
   GLK-T-REPEAT
   MKDF:OFF
   PTXTC:CLEAN
   T-REPORT ;

GLK-RUN

;package
s" gpu-leak-test: ok" type cr
