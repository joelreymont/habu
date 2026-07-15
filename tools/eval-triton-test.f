\ tools/eval-triton-test.f - acceptance for the SHIPPED-evidence migration into the
\ typed BENCH store + report path (dot habu-migrate-saxpy-and-2c02dd5a).
\
\ Pins the dot's acceptance:
\ (a) the shipped SAXPY FP32 and Habu-MMM/Triton TF32 rows render BYTE-STABLY as report
\     rows exposing workload/shape/protocol/baseline/cache-state/unit/both policies (the
\     schema carries no artifact id - the persist dot deferred that), and the historical
\     FP32-vs-TF32 pair still renders as separately-labelled source evidence (spol=exact|
\     bpol=rel);
\ (b) each side's comparability is re-derived at this layer: SAXPY exact/exact and MMM
\     TF32 rel/rel are comparable; the historical exact-vs-rel pair is NOT;
\ (c) MIGRATE persists both comparable results through the sealed store; each is found
\     under its exact key and REPLAYS (get -> decode -> re-encode) byte-for-byte to its
\     committed canonical row (same digests as maki/competitive-store-test.f);
\ (d) changing one exact-key field (cache warm->cold) INVALIDATES lookup - GET on the
\     changed key MISSES while the exact key HITS (the store's whole-render key discipline);
\ (e) the checked importer REFUSES the historical incomparable pair as a competitive
\     result (IMPORT-GFLOPS-RESULT throws E-BENCH-INCOMPARABLE), with the comparable MMM
\     import as the resolving positive (no throw).
\ Fixture numbers are the real device measurements (docs/eval-triton.md,
\ tools/ptx/perf-rows.tsv), milli-units x1000. Every negative is paired with a resolving
\ positive control (per LESSONS).

require lib/test.f
require lib/string.f
require tools/eval-triton.f

package EVAL-TRITON

\ ---- importer throw triggers (tick form; top-level tests cannot push [: ;]) ----
: TRY-IMPORT-HISTORICAL ( -- )  HISTORICAL-FP32-VS-TF32 IMPORT-GFLOPS-RESULT ;
: TRY-IMPORT-MMM        ( -- )  HABU-MMM-TF32           IMPORT-GFLOPS-RESULT ;
: TRY-IMPORT-SAXPY      ( -- )  SAXPY-FP32              IMPORT-GBPS-RESULT ;
\ mechanism control (NOT shipped evidence): a synthetic exact-vs-relative gbps pair, so
\ the gbps importer's comparability gate is pinned symmetrically with the gflops one.
: SYN-INCOMPARABLE-GBPS ( -- BENCH:comparison-gbps )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:RELATIVE
   64209 BENCH:>GBPS 63000 BENCH:>GBPS BENCH:COMPARE-GBPS ;
: TRY-IMPORT-SYN-GBPS ( -- )  SYN-INCOMPARABLE-GBPS IMPORT-GBPS-RESULT ;

T-RESET

\ ==== (a) report goldens: byte-stable canonical rows (commit these) ============
SAXPY-REPORT$
   s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|base=63000"
   T$=
MMM-REPORT$
   s" bench/v1|wl=gemm|sh=sq2048|pr=cuda-events|bl=triton|cache=warm|spol=rel|bpol=rel|unit=gflops|subj=884889|base=1890500"
   T$=
\ the historical pair renders as separately-labelled source evidence (spol=exact|bpol=rel).
HISTORICAL-REPORT$
   s" bench/v1|wl=gemm|sh=sq2048|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=rel|unit=gflops|subj=380501|base=1890500"
   T$=

\ ==== (b) comparability re-derived at the migration layer ======================
SAXPY-COMPARABLE?      TTRUE      \ FP32 exact/exact       -> comparable
MMM-COMPARABLE?        TTRUE      \ TF32 rel/rel           -> comparable (resolving positive)
HISTORICAL-COMPARABLE? TFALSE     \ FP32 exact vs TF32 rel -> incomparable (the confusion)

\ ==== (c) persist through the sealed store, then replay byte-for-byte ===========
MIGRATE
SAXPY-FOUND? TTRUE               \ both comparable results are stored under their exact key
MMM-FOUND?   TTRUE
RELOAD-SAXPY$
   s" bench/v1|wl=saxpy|sh=n1m|pr=cuda-events|bl=triton|cache=warm|spol=exact|bpol=exact|unit=gbps|subj=64209|base=63000|schema=bench/v1|mspol=exact|mbpol=exact|munit=gbps|digest=2D9DB051DDB46CC8|promo=comparable"
   T$=
RELOAD-MMM$
   s" bench/v1|wl=gemm|sh=sq2048|pr=cuda-events|bl=triton|cache=warm|spol=rel|bpol=rel|unit=gflops|subj=884889|base=1890500|schema=bench/v1|mspol=rel|mbpol=rel|munit=gflops|digest=285B2855A495256A|promo=comparable"
   T$=

\ ==== (d) changing one exact-key field invalidates lookup ======================
SAXPY-COLD-FOUND? TFALSE         \ cache warm->cold changes the exact key -> lookup MISS
SAXPY-FOUND?      TTRUE          \ resolving positive: the exact key still HITS

\ ==== (e) the checked importer refuses the historical incomparable pair ========
' TRY-IMPORT-HISTORICAL E-BENCH-INCOMPARABLE TTHROWS   \ never loads as a competitive result
' TRY-IMPORT-MMM        0                    TTHROWS   \ resolving positive: a comparable result imports
' TRY-IMPORT-SYN-GBPS   E-BENCH-INCOMPARABLE TTHROWS   \ the gbps importer gates comparability too
' TRY-IMPORT-SAXPY      0                    TTHROWS   \ resolving positive: the comparable gbps result imports

BENCH:BENCH-STORE-RESET
;package

T-REPORT
