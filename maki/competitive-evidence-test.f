\ maki/competitive-evidence-test.f - acceptance for the checked per-side competitive
\ EVIDENCE row + metric UNITS vocabulary (maki/competitive-evidence.f, dot
\ habu-v2-competitive-evidence-5d07d471).
\
\ Pins the dot's acceptance:
\   - the migrated flagship rows render byte-stably (the GEMM 3026.6 GFLOP/s =
\     1.60x Triton row and the SAXPY-V4 64.209 GB/s = Triton 63.0 row), each an
\     exact versioned golden; a rebuild + re-render is byte-identical (replay);
\   - every metric UNIT (ns/ms/gflops/gbps/bytes/count/watts) renders;
\   - a comparison ROW refuses a mismatched numeric DOMAIN (the historical
\     Habu-FP32-exact vs Triton-TF32-relative confusion) - runtime verdict 0 AND
\     the E-CEVID-INCOMPARABLE throw - each with a resolving positive;
\   - cache-state is an explicit field: cold vs warm alters the rendered key;
\   - a reading whose unit is outside its field category is the E-CEVID-UNIT throw;
\   - over-capacity / oversized-row throws are the named E-CEVID codes;
\   - the numeric-policy / raw-n / identity-slot confusions are STATIC checker
\     rejects (CHECK-QUIET-CANDIDATE! 0), each with a resolving positive.
\
\ Fixture numbers are the real device measurements: MMM-WIDE-B-M4-S1 3026.577
\ GFLOP/s vs Triton 1890.5 (tools/ptx/perf-rows.tsv, orin-nx-25w-918mhz, 2048^3,
\ both TF32/relative); SAXPY-V4 64.209 GB/s vs Triton 63.0 (both FP32/exact);
\ Triton GEMM time_ms=272.6 (docs/eval-triton.md); GEMM smem 50176 B; SAXPY bytes
\ 200*3*2^20*4 = 2516582400; ns figure the step-1 blocked 2048^3 gpu_elapsed_ns.
\ Rate units and ms are milli-scaled (x1000); ns/bytes/count/watts are natural.

require lib/test.f
require lib/string.f
require test/checker-assert.f       \ CHECK-QUIET-CANDIDATE! for the static-reject negatives
require maki/evidence/schema.f      \ EVID:prec-class for the numeric-policy-confusion negative
require maki/competitive-evidence.f

package CEVID

\ ---- stable copy (the shared SB render is overwritten by the next render) --------
1024 constant CT-CAP
create CT-BUF CT-CAP allot  variable CT-BU
: CT-COPY ( ptr u8 n -- ) {: a:ptr u:n :}  a CT-BUF u BYTE-COPY  u CT-BU ! ;
: CT-BUF$ ( -- ptr u8 n )  CT-BUF CT-BU @ ;

\ ==== the migrated flagship evidence rows (real device numbers, milli/natural) ====
\ GEMM subject: Habu MMM-WIDE-B-M4-S1 TF32 tensor-core tile, the 3026.6 = 1.60x flagship.
: GEMM-HABU ( -- evidence )
   BENCH-WORKLOAD:GEMM CEVID-REVISION:MMM-WIDE-B BENCH-SHAPE:SQ2048 NPOL-DOM:RELATIVE
   TARGET:SM87 CEVID-COMPILER:HABU-PTXAS BENCH-CACHE--STATE:WARM
   BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON ROW
   3026577 >GFLOPS THR!
   30 >LAUNCHES LAUNCH!
   50176 >BYTES MEM!
   25 >WATTS ENERGY! ;
\ GEMM baseline: Triton autotuned TF32 tl.dot, 1890.5 GFLOP/s, time_ms=272.6.
: GEMM-TRITON ( -- evidence )
   BENCH-WORKLOAD:GEMM CEVID-REVISION:TRITON-TUNED BENCH-SHAPE:SQ2048 NPOL-DOM:RELATIVE
   TARGET:SM87 CEVID-COMPILER:TRITON-351 BENCH-CACHE--STATE:WARM
   BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON ROW
   272600 >MS LAT!
   1890500 >GFLOPS THR!
   30 >LAUNCHES LAUNCH!
   25 >WATTS ENERGY! ;
\ SAXPY subject: Habu SAXPY-V4 64.209 GB/s (FP32 exact).
: SAXPY-HABU ( -- evidence )
   BENCH-WORKLOAD:SAXPY CEVID-REVISION:SAXPY-V4 BENCH-SHAPE:N1M NPOL-DOM:EXACT
   TARGET:SM87 CEVID-COMPILER:HABU-PTXAS BENCH-CACHE--STATE:WARM
   BENCH-PROTOCOL:WALLCLOCK BENCH-BASELINE:TRITON ROW
   64209 >GBPS THR!
   2516582400 >BYTES BYT!
   200 >LAUNCHES LAUNCH!
   25 >WATTS ENERGY! ;
\ SAXPY baseline: Triton JIT 63.0 GB/s (FP32 exact).
: SAXPY-TRITON ( -- evidence )
   BENCH-WORKLOAD:SAXPY CEVID-REVISION:TRITON-JIT BENCH-SHAPE:N1M NPOL-DOM:EXACT
   TARGET:SM87 CEVID-COMPILER:TRITON-351 BENCH-CACHE--STATE:WARM
   BENCH-PROTOCOL:WALLCLOCK BENCH-BASELINE:TRITON ROW
   63000 >GBPS THR!
   2516582400 >BYTES BYT!
   200 >LAUNCHES LAUNCH!
   25 >WATTS ENERGY! ;

\ the SAME GEMM subject but cache=cold: one exact-key field changed.
: GEMM-HABU-COLD ( -- evidence )
   BENCH-WORKLOAD:GEMM CEVID-REVISION:MMM-WIDE-B BENCH-SHAPE:SQ2048 NPOL-DOM:RELATIVE
   TARGET:SM87 CEVID-COMPILER:HABU-PTXAS BENCH-CACHE--STATE:COLD
   BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON ROW
   3026577 >GFLOPS THR!  30 >LAUNCHES LAUNCH!  50176 >BYTES MEM!  25 >WATTS ENERGY! ;

\ the historical Habu blocked FP32 tile (exact) - incomparable-by-policy vs Triton TF32.
: GEMM-HABU-FP32 ( -- evidence )
   BENCH-WORKLOAD:GEMM CEVID-REVISION:MM-BLOCKED BENCH-SHAPE:SQ2048 NPOL-DOM:EXACT
   TARGET:SM87 CEVID-COMPILER:HABU-PTXAS BENCH-CACHE--STATE:WARM
   BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON ROW
   441800 >GFLOPS THR!  30 >LAUNCHES LAUNCH!  25 >WATTS ENERGY! ;

\ a row exercising the ns unit + the all-na default readings (real corpus figure).
: NS-ROW ( -- evidence )
   BENCH-WORKLOAD:GEMM CEVID-REVISION:MM-BLOCKED BENCH-SHAPE:SQ2048 NPOL-DOM:EXACT
   TARGET:SM87 CEVID-COMPILER:HABU-PTXAS BENCH-CACHE--STATE:WARM
   BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON ROW
   1354517944 >NS LAT! ;

\ ---- comparability probes ----------------------------------------------------
: GEMM-CMP ( -- bool )  GEMM-HABU GEMM-TRITON COMPARABLE? ;      \ both relative -> comparable
: SAXPY-CMP ( -- bool )  SAXPY-HABU SAXPY-TRITON COMPARABLE? ;   \ both exact    -> comparable
: BAD-CMP ( -- bool )  GEMM-HABU-FP32 GEMM-TRITON COMPARABLE? ;  \ exact vs relative -> NOT comparable

\ ---- fail-closed triggers ----------------------------------------------------
: TRY-INCOMPARABLE ( -- )  GEMM-HABU-FP32 GEMM-TRITON RENDER-PAIR 2drop ;  \ incomparable pair as a row
: TRY-LAT-RATE ( -- )  SAXPY-HABU 63000 >GBPS LAT! drop ;   \ a rate reading in the latency (duration) slot
: TRY-LAT-NS-OK ( -- )  SAXPY-HABU 100 >NS LAT! drop ;      \ a duration reading in the latency slot: OK
: TRY-CAP-HI ( -- )  1000000000000001 >GFLOPS drop ;        \ over the value capacity
: TRY-CAP-NEG ( -- ) -1 >GBPS drop ;                        \ negative reading value
create BIGROW 600 allot
: TRY-ROW ( -- )  BIGROW 600 ROW-STABLE 2drop ;             \ 600 > EV-ROW-CAP (512)

T-RESET

\ ==== byte goldens: the migrated flagship rows (commit these) ==================
GEMM-HABU RENDER
   s" cevid/v1|wl=gemm|rev=mmm-wide-b-m4-s1|sh=sq2048|pol=rel|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=cuda-events|bl=triton|lat=na:not-measured|thr=3026577:gflops|byt=na:not-measured|launch=30:count|mem=50176:bytes|energy=25:watts"
   T$=
GEMM-TRITON RENDER
   s" cevid/v1|wl=gemm|rev=triton-autotuned|sh=sq2048|pol=rel|tgt=sm_87|comp=triton-3.5.1|cache=warm|pr=cuda-events|bl=triton|lat=272600:ms|thr=1890500:gflops|byt=na:not-measured|launch=30:count|mem=na:not-measured|energy=25:watts"
   T$=
SAXPY-HABU RENDER
   s" cevid/v1|wl=saxpy|rev=saxpy-v4|sh=n1m|pol=exact|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=wallclock|bl=triton|lat=na:not-measured|thr=64209:gbps|byt=2516582400:bytes|launch=200:count|mem=na:not-measured|energy=25:watts"
   T$=
SAXPY-TRITON RENDER
   s" cevid/v1|wl=saxpy|rev=triton-jit|sh=n1m|pol=exact|tgt=sm_87|comp=triton-3.5.1|cache=warm|pr=wallclock|bl=triton|lat=na:not-measured|thr=63000:gbps|byt=2516582400:bytes|launch=200:count|mem=na:not-measured|energy=25:watts"
   T$=
\ the ns unit + all-na default readings.
NS-ROW RENDER
   s" cevid/v1|wl=gemm|rev=mm-cp-async-blocked|sh=sq2048|pol=exact|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=cuda-events|bl=triton|lat=1354517944:ns|thr=na:not-measured|byt=na:not-measured|launch=na:not-measured|mem=na:not-measured|energy=na:not-measured"
   T$=

\ ==== replay is byte-stable: rebuild + re-render is byte-identical =============
GEMM-HABU RENDER CT-COPY
GEMM-HABU RENDER CT-BUF$ T$=
SAXPY-HABU RENDER CT-COPY
SAXPY-HABU RENDER CT-BUF$ T$=

\ ==== the comparison ROW renders byte-stably (comparable pair) =================
GEMM-HABU GEMM-TRITON RENDER-PAIR
   s" cevid-cmp/v1|comparable=yes|subj=cevid/v1|wl=gemm|rev=mmm-wide-b-m4-s1|sh=sq2048|pol=rel|tgt=sm_87|comp=ptxas-12.6|cache=warm|pr=cuda-events|bl=triton|lat=na:not-measured|thr=3026577:gflops|byt=na:not-measured|launch=30:count|mem=50176:bytes|energy=25:watts|base=cevid/v1|wl=gemm|rev=triton-autotuned|sh=sq2048|pol=rel|tgt=sm_87|comp=triton-3.5.1|cache=warm|pr=cuda-events|bl=triton|lat=272600:ms|thr=1890500:gflops|byt=na:not-measured|launch=30:count|mem=na:not-measured|energy=25:watts"
   T$=
GEMM-HABU GEMM-TRITON RENDER-PAIR CT-COPY
GEMM-HABU GEMM-TRITON RENDER-PAIR CT-BUF$ T$=

\ ==== cache-state is an explicit field: cold vs warm alters the key ============
GEMM-HABU RENDER CT-COPY
GEMM-HABU-COLD RENDER CT-BUF$ T$<>

\ ==== the numeric-policy DOMAIN pairing verdict ===============================
GEMM-CMP  TTRUE      \ both relative (Habu TF32 vs Triton TF32) -> comparable
SAXPY-CMP TTRUE      \ both exact (FP32) -> comparable
BAD-CMP   TFALSE     \ exact FP32 vs relative TF32 (the historical confusion) -> verdict 0

\ ==== named fail-closed throws ================================================
' TRY-INCOMPARABLE E-CEVID-INCOMPARABLE TTHROWS  \ a mismatched-domain pair cannot form a comparison row
' TRY-LAT-RATE     E-CEVID-UNIT         TTHROWS  \ a rate reading in the latency slot: rejected
' TRY-LAT-NS-OK    0                     TTHROWS  \ a duration reading in the latency slot: accepted
' TRY-CAP-HI       E-CEVID-CAP          TTHROWS  \ over the value capacity
' TRY-CAP-NEG      E-CEVID-CAP          TTHROWS  \ negative reading value
' TRY-ROW          E-CEVID-ROW          TTHROWS  \ oversized row over the stable buffer

\ ==== STATIC checker rejects (CHECK-QUIET-CANDIDATE!): each 0 + resolving -1 ====
\ resolving positive control: every slot correct -> certifies.
s" P0 ( BENCH:workload CEVID:revision BENCH:shape NPOL:dom CAD-KIND:target-id CEVID:compiler BENCH:cache-state BENCH:protocol BENCH:baseline -- CEVID:evidence ) CEVID:ROW"
   CHECK-QUIET-CANDIDATE! -1 T=
\ FP32/TF32 policy confusion: a precision class (EVID:prec-class) in the numeric-policy slot.
s" N-POL ( BENCH:workload CEVID:revision BENCH:shape EVID:prec-class CAD-KIND:target-id CEVID:compiler BENCH:cache-state BENCH:protocol BENCH:baseline -- CEVID:evidence ) CEVID:ROW"
   CHECK-QUIET-CANDIDATE! 0 T=
s" P-POL ( BENCH:workload CEVID:revision BENCH:shape NPOL:dom CAD-KIND:target-id CEVID:compiler BENCH:cache-state BENCH:protocol BENCH:baseline -- CEVID:evidence ) CEVID:ROW"
   CHECK-QUIET-CANDIDATE! -1 T=
\ raw n where a nominal workload id is required.
s" N-RAW ( n CEVID:revision BENCH:shape NPOL:dom CAD-KIND:target-id CEVID:compiler BENCH:cache-state BENCH:protocol BENCH:baseline -- CEVID:evidence ) CEVID:ROW"
   CHECK-QUIET-CANDIDATE! 0 T=
\ identity-slot swap: a baseline id where the workload id is required.
s" N-ID ( BENCH:baseline CEVID:revision BENCH:shape NPOL:dom CAD-KIND:target-id CEVID:compiler BENCH:cache-state BENCH:protocol BENCH:baseline -- CEVID:evidence ) CEVID:ROW"
   CHECK-QUIET-CANDIDATE! 0 T=

;package
T-REPORT
