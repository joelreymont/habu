\ maki/competitive-report-test.f - acceptance suite for the checked BENCH
\ competitive-comparison schema (dot habu-define-checked-bench-5341ffc8).
\
\ Pins the dot's acceptance: byte-stable canonical renders for the SAXPY FP32 and
\ Habu-MMM/Triton TF32 rows; every exact-key field (and cold/warm) alters the
\ rendered key; unavailable readings render their named absence reason; the
\ numeric-policy pairing verdict rejects the historical Habu-FP32-vs-Triton-TF32
\ confusion (verdict 0) with a resolving positive; the identity / raw-n / cache /
\ unit / precision confusions are checker rejects (CHECK-QUIET-CANDIDATE! 0) each
\ with a resolving positive; capacity overflows throw the named E-BENCH codes.
\ Every negative is paired with a resolving positive control (per LESSONS).
\
\ Fixture numbers are the real device measurements: SAXPY-V4 64.209 GB/s vs Triton
\ 63.0 GB/s (docs/eval-triton.md, tools/ptx/perf-rows.tsv), Habu MMM TF32 884.889
\ GFLOP/s vs Triton TF32 1890.5 GFLOP/s, and the historical Habu blocked FP32
\ 380.5 GFLOP/s vs Triton TF32 1890.5 GFLOP/s - milli-units (x1000).

require lib/test.f
require lib/string.f
require test/checker-assert.f       \ CHECK-QUIET-CANDIDATE! for the type-reject negatives
require maki/evidence/schema.f      \ EVID:prec-class for the FP32/TF32 policy-confusion negative
require maki/competitive-report.f

package BENCH

\ ---- stable copy (the shared SB render is overwritten by the next render) -----
256 constant CT-CAP
create CT-BUF CT-CAP allot  variable CT-BU
: CT-COPY ( ptr u8 n -- ) {: a:ptr u:n :}  a CT-BUF u BYTE-COPY  u CT-BU ! ;
: CT-BUF$ ( -- ptr u8 n )  CT-BUF CT-BU @ ;

\ ---- build-and-render helpers (product never crosses an interpret boundary) ---
\ ( 4 ids, cache, subj-policy, base-policy, subj-milli, base-milli -- rendered row )
: GBPS-ROW$ ( workload shape protocol baseline cache-state NPOL:dom NPOL:dom n n -- ptr u8 n )
   {: sm:n bm:n :}  sm >GBPS bm >GBPS COMPARE-GBPS RENDER-GBPS ;
: GFLOPS-ROW$ ( workload shape protocol baseline cache-state NPOL:dom NPOL:dom n n -- ptr u8 n )
   {: sm:n bm:n :}  sm >GFLOPS bm >GFLOPS COMPARE-GFLOPS RENDER-GFLOPS ;

\ base GB/s row = the SAXPY FP32 golden; each variant below changes ONE field.
: G-BASE$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63000 GBPS-ROW$ ;
: G-WL$ ( -- ptr u8 n )
   BENCH-WORKLOAD:GEMM BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63000 GBPS-ROW$ ;
: G-SH$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:SQ2048 BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63000 GBPS-ROW$ ;
: G-PR$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:WALLCLOCK BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63000 GBPS-ROW$ ;
: G-BL$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:CUBLAS
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63000 GBPS-ROW$ ;
: G-CACHE$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:COLD NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63000 GBPS-ROW$ ;
: G-SPOL$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:RELATIVE NPOL-DOM:EXACT 64209 63000 GBPS-ROW$ ;
: G-BPOL$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:RELATIVE 64209 63000 GBPS-ROW$ ;
: G-SUBJ$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64210 63000 GBPS-ROW$ ;
: G-BASEV$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63001 GBPS-ROW$ ;
\ same identity/policy/values as the base but the GFLOP/s unit: only the unit token differs.
: G-UNIT$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 63000 GFLOPS-ROW$ ;

\ the Habu-MMM / Triton TF32 golden (both relative).
: MMM$ ( -- ptr u8 n )
   BENCH-WORKLOAD:GEMM BENCH-SHAPE:SQ2048 BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:RELATIVE NPOL-DOM:RELATIVE 884889 1890500 GFLOPS-ROW$ ;

\ an absent baseline reading renders its named reason.
: G-ABS$ ( -- ptr u8 n )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:COLD NPOL-DOM:EXACT NPOL-DOM:EXACT
   64209 >GBPS BENCH-ABSENCE:DEVICE-GATED GBPS-ABSENT COMPARE-GBPS RENDER-GBPS ;

\ a normal row survives the stable copy (positive capacity control).
: OK-ROW$ ( -- ptr u8 n )  G-BASE$ ROW-STABLE ;

\ ---- comparison builders for the runtime pairing verdict ---------------------
: SAXPY-CMP ( -- bool )     \ both exact  -> comparable
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT 64209 >GBPS 63000 >GBPS COMPARE-GBPS GBPS-COMPARABLE? ;
: MMM-CMP ( -- bool )       \ both relative (Habu TF32 vs Triton TF32) -> comparable
   BENCH-WORKLOAD:GEMM BENCH-SHAPE:SQ2048 BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:RELATIVE NPOL-DOM:RELATIVE 884889 >GFLOPS 1890500 >GFLOPS COMPARE-GFLOPS GFLOPS-COMPARABLE? ;
: BAD-CMP ( -- bool )       \ Habu FP32 (exact) vs Triton TF32 (relative) -> NOT comparable
   BENCH-WORKLOAD:GEMM BENCH-SHAPE:SQ2048 BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:RELATIVE 380501 >GFLOPS 1890500 >GFLOPS COMPARE-GFLOPS GFLOPS-COMPARABLE? ;

\ ---- capacity fail-closed triggers -------------------------------------------
: TRY-CAP-HI ( -- )  1000000001 >GBPS drop ;      \ over the milli-unit capacity
: TRY-CAP-NEG ( -- ) -1 >GFLOPS drop ;            \ negative throughput
create BIGROW 300 allot
: TRY-ROW ( -- )  BIGROW 300 ROW-STABLE 2drop ;   \ 300 > BENCH-ROW-CAP (256)

T-RESET

\ ==== byte goldens: SAXPY FP32 and Habu-MMM/Triton TF32 (commit these) =========
G-BASE$
   s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|base=63000"
   T$=
MMM$
   s" bench/v1|wl=gemm|sh=sq2048|pr=cuda-events|bl=triton|cache=warm|spol=rel|bpol=rel|unit=gflops|subj=884889|base=1890500"
   T$=
\ the stable copy of a normal row is byte-identical (positive capacity control).
OK-ROW$
   s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|base=63000"
   T$=

\ ==== every exact-key field (and cold/warm) alters the rendered key ============
G-BASE$ CT-COPY                  \ pin the base row; each variant must differ
G-WL$    CT-BUF$ T$<>            \ workload
G-SH$    CT-BUF$ T$<>            \ shape
G-PR$    CT-BUF$ T$<>            \ protocol
G-BL$    CT-BUF$ T$<>            \ baseline
G-CACHE$ CT-BUF$ T$<>            \ cache state (cold/warm)
G-SPOL$  CT-BUF$ T$<>            \ subject numeric policy
G-BPOL$  CT-BUF$ T$<>            \ baseline numeric policy
G-UNIT$  CT-BUF$ T$<>            \ metric unit token
G-SUBJ$  CT-BUF$ T$<>            \ subject throughput value
G-BASEV$ CT-BUF$ T$<>            \ baseline throughput value

\ ==== unavailable reading renders its named absence reason =====================
G-ABS$
   s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=cold|spol=exact|bpol=exact|unit=gbps|subj=64209|base=na:device-gated"
   T$=

\ ==== runtime pairing verdict: numeric-policy witnesses must match =============
SAXPY-CMP TTRUE      \ both exact           -> comparable
MMM-CMP   TTRUE      \ both relative (TF32)  -> comparable (resolving positive)
BAD-CMP   TFALSE     \ exact FP32 vs relative TF32 (the historical confusion) -> verdict 0

\ ==== type-reject confusions (CHECK-QUIET-CANDIDATE!): each 0 + resolving -1 ====
\ resolving positive control: every slot correct -> certifies.
s" P0 ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! -1 T=
\ FP32/TF32 policy confusion: a precision class (EVID:prec-class) in the numeric-policy slot.
s" N-POL ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state EVID:prec-class NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! 0 T=
s" P-POL ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! -1 T=
\ identity-slot swap: a workload id where the baseline id is expected.
s" N-ID ( BENCH:workload BENCH:shape BENCH:protocol BENCH:workload BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! 0 T=
s" P-ID ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! -1 T=
\ raw n where a nominal id is required.
s" N-RAW ( n BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! 0 T=
s" P-RAW ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! -1 T=
\ cache-state confusion: a raw n where the closed cache-state sum is required.
s" N-CACHE ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline n NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! 0 T=
s" P-CACHE ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! -1 T=
\ throughput-unit confusion: a GFLOP/s reading where the GB/s baseline slot is expected.
s" N-UNIT ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gflops -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! 0 T=
s" P-UNIT ( BENCH:workload BENCH:shape BENCH:protocol BENCH:baseline BENCH:cache-state NPOL:dom NPOL:dom BENCH:gbps BENCH:gbps -- BENCH:comparison-gbps ) BENCH:COMPARE-GBPS"
   CHECK-QUIET-CANDIDATE! -1 T=

\ ==== capacity overflows throw the named E-BENCH codes =========================
' TRY-CAP-HI  E-BENCH-CAP TTHROWS
' TRY-CAP-NEG E-BENCH-CAP TTHROWS
' TRY-ROW     E-BENCH-ROW TTHROWS

;package
T-REPORT
