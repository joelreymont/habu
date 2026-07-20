\ maki/lower/launch-leak-test.f - HOST-ONLY leak proof for the migrated lower-launch lifecycles.
\
\ Drives the REAL migrated maki/lower/launch.f acquire words off-device through the
\ shared recording fake (maki/cuda-run-fake.f): the single-region lifecycle
\ (LLA-CTX-OPEN + LLA-MOD-OPEN + LLA-ALLOC-UPLOAD under one scope) and the whole-model
\ TWO-LEVEL ownership (the outer model scope owns the primary context and each region's
\ output buffer; a per-region inner scope owns that region's module). It proves both:
\   - the single-region scope releases exactly the owned prefix, reversed, on a failure
\     at each acquisition point, while the pre-migration sequence releases nothing; and
\   - across regions the model keeps ONLY the context + node buffers alive (each region's
\     module is released at region end) and unwinds them all in reverse at model end.
\ No launch/dispatch runs (it acquires nothing), so no GPU or libcuda is touched.

require lib/test.f
require maki/lower/launch.f
require maki/cuda-run-fake.f

package MAKI                         \ reopen for LLA-CTX-OPEN / LLA-MOD-OPEN / LLA-ALLOC-UPLOAD + LLA-* globals

\ ---- minimal single-region staging (2 inputs of 1 elem, 1-elem output) --------
: LKT-STAGE ( -- )
   2 LLA-NIN !  1 LLA-ELEMS !
   1 LLA-IN-ELEMS !  1 LLA-IN-ELEMS 1 cells + ! ;

\ single-region acquire chain (ctx + module + input/output buffers); stops before launch
: LKT-SR-BODY ( -- )
   LLA-CTX-OPEN
   s" region.cubin" LLA-MOD-OPEN
   LLA-ELEMS @ 4 * LLA-ALLOC-UPLOAD ;

\ pre-migration single-region acquire (no ownership transfer): a failure leaks the prefix.
\ Mirrors LLA-CTX-OPEN + LLA-MOD-OPEN + LLA-ALLOC-UPLOAD's per-input (alloc, upload) order.
: LKT-SR-BASE ( -- )
   MKD:OPEN  0 MKD:CUINIT CUDA:RC0
   LLA-DEV 0 >IDX MKD:CUDEVICEGET CUDA:RC0
   LLA-CTX LLA-DEV @ >CUDA-DEV MKD:CUDEVICEPRIMARYCTXRETAIN CUDA:RC0
   LLA-CTX @ >CUDA-CTX MKD:CUCTXSETCURRENT CUDA:RC0
   s" region.cubin" LLA-PATH >CSTR
   LLA-MOD LLA-PATH MKD:CUMODULELOAD CUDA:RC0
   LLA-FUNC LLA-MOD @ >CUDA-MOD LLA-FN MKD:CUMODULEGETFUNCTION CUDA:RC0
   0 LLA-DBUF-I 4 >LEN MKD:CUMEMALLOC CUDA:RC0
   0 LLA-DBUF-I @ >CUDA-DEVPTR  0 LLA-HIN-I  4 >LEN MKD:CUMEMCPYHTOD CUDA:RC0
   1 LLA-DBUF-I 4 >LEN MKD:CUMEMALLOC CUDA:RC0
   1 LLA-DBUF-I @ >CUDA-DEVPTR  1 LLA-HIN-I  4 >LEN MKD:CUMEMCPYHTOD CUDA:RC0 ;

: LKT-SR-SCOPED ( -- )  [: LKT-SR-BODY ;] CUDA-SCOPE:SCOPE ;

: LKT-T-GETFUNC ( -- )               \ getfunction fails: module + ctx unwound (nothing in base)
   LKT-STAGE
   MKDF:RESET  7 MKDF:FUNC-RC !
   [: LKT-SR-BASE ;] E-CUDA TTHROWSQ  MKDF:RN@ 0 T=
   MKDF:RESET  7 MKDF:FUNC-RC !
   [: LKT-SR-SCOPED ;] E-CUDA TTHROWSQ
   MKDF:RN@ 2 T=  0 MKDF:REL@ MKDF:MOD-H T=  1 MKDF:REL@ MKDF:DEV-H T=
   CUDA-SCOPE:DEPTH 0 T= ;

: LKT-T-OUTALLOC ( -- )              \ output alloc (3rd) fails: both inputs + module + ctx unwound, reversed
   LKT-STAGE
   MKDF:RESET  2 MKDF:FAIL-A !  5 MKDF:A-RC !
   [: LKT-SR-SCOPED ;] E-CUDA TTHROWSQ
   MKDF:RN@ 4 T=
   0 MKDF:REL@ 1 MKDF:ALLOC-H T=  1 MKDF:REL@ 0 MKDF:ALLOC-H T=
   2 MKDF:REL@ MKDF:MOD-H T=  3 MKDF:REL@ MKDF:DEV-H T=
   CUDA-SCOPE:DEPTH 0 T= ;

: LKT-T-HTOD ( -- )                  \ first upload copy throws: input0 + module + ctx unwound
   LKT-STAGE
   MKDF:RESET  9 MKDF:HTOD-RC !
   [: LKT-SR-BASE ;] E-CUDA TTHROWSQ  MKDF:RN@ 0 T=
   MKDF:RESET  9 MKDF:HTOD-RC !
   [: LKT-SR-SCOPED ;] E-CUDA TTHROWSQ
   MKDF:RN@ 3 T=  0 MKDF:REL@ 0 MKDF:ALLOC-H T=  1 MKDF:REL@ MKDF:MOD-H T=  2 MKDF:REL@ MKDF:DEV-H T=
   CUDA-SCOPE:DEPTH 0 T= ;

\ ---- whole-model two-level ownership: ctx + node buffers outlive per-region modules ----
variable LKT-NODEBUF
: LKT-ALLOC-NODE ( -- )              \ mirror MDL-ALLOC-OUT: allocate a node buffer owned by the OUTER scope
   LKT-NODEBUF 4 >LEN MKD:CUMEMALLOC CUDA:RC0  LKT-NODEBUF @ >CUDA-DEVPTR CUDA-SCOPE:OWN-DEVPTR ;
: LKT-WM-BODY ( -- )                 \ mirror LOWER-MODEL-RUN: outer owns ctx + node buffers, per-region scope owns module
   LLA-CTX-OPEN                                          \ ctx owned by the model (outer) scope
   LKT-ALLOC-NODE                                        \ region 0 output buffer -> outer scope
   [: s" r0.cubin" LLA-MOD-OPEN ;] CUDA-SCOPE:SCOPE      \ region 0 module owned + freed here
   MKDF:RN@ 1 T=  0 MKDF:REL@ MKDF:MOD-H T=              \ module0 released at region-0 scope exit
   CUDA-SCOPE:DEPTH 2 T=                                 \ ctx + node buffer 0 still alive
   LKT-ALLOC-NODE                                        \ region 1 output buffer -> outer scope
   [: s" r1.cubin" LLA-MOD-OPEN ;] CUDA-SCOPE:SCOPE      \ region 1 module owned + freed here
   MKDF:RN@ 2 T=  1 MKDF:REL@ MKDF:MOD-H T=              \ module1 released at region-1 scope exit
   CUDA-SCOPE:DEPTH 3 T= ;                               \ ctx + both node buffers alive between regions
: LKT-T-MODEL ( -- )
   MKDF:RESET
   [: LKT-WM-BODY ;] CUDA-SCOPE:SCOPE                    \ the model scope
   MKDF:RN@ 5 T=                                         \ 2 modules (per region) + 2 node buffers + ctx
   2 MKDF:REL@ 1 MKDF:ALLOC-H T=  3 MKDF:REL@ 0 MKDF:ALLOC-H T=  4 MKDF:REL@ MKDF:DEV-H T=  \ node bufs then ctx, reversed
   CUDA-SCOPE:DEPTH 0 T= ;

: LKT-RUN ( -- )
   T-RESET
   MKDF:ON
   LKT-T-GETFUNC
   LKT-T-OUTALLOC
   LKT-T-HTOD
   LKT-T-MODEL
   MKDF:OFF
   T-REPORT ;

LKT-RUN

;package
s" launch-leak-test: ok" type cr
