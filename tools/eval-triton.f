\ tools/eval-triton.f - migrate the SHIPPED SAXPY and GEMM competitive evidence into
\ the typed BENCH store + report path (dot habu-migrate-saxpy-and-2c02dd5a, the final
\ leaf of epic habu-v2-competitive-evidence-5d07d471).
\
\ The measured Habu-PTX-vs-Triton competitive claims used to live ONLY as prose in
\ docs/eval-triton.md plus the Habu-side profile registry (tools/ptx/perf-rows.tsv):
\ nothing typed held the comparison, so a reader had to TRUST the doc that a Habu FP32
\ GEMM number and a Triton TF32 GEMM number were not one comparable result. This module
\ carries each shipped comparison as a typed BENCH value (maki/competitive-report.f) and
\ drives it through the SEALED store (maki/competitive-store.f) using ONLY the public
\ typed writers/readers - it is an external consumer of the store seal (package
\ EVAL-TRITON, not a reopen of package BENCH; no access to the private raw appender), so
\ the migration is itself the proof that the public typed surface is sufficient to
\ persist, report, and replay the shipped evidence byte-stably.
\
\ MIGRATED RESULTS (both legs measured same-session on the Orin; warm cache = one warmup
\ launch then CUDA-event timing; readings are milli-units, x1000):
\   SAXPY-FP32     Habu SAXPY-V4 64.209 GB/s   vs Triton 63.0 GB/s      - FP32 exact/exact, comparable
\   HABU-MMM-TF32  Habu MMM 884.889 GFLOP/s    vs Triton 1890.5 GFLOP/s - TF32 rel/rel,     comparable
\ Both shipped comparisons have BOTH legs measured, so neither carries a na:<absence>
\ leg; the absent-leg render path is proven by the sibling goldens
\ (maki/competitive-store-test.f ABS-GOLD$) - we do not fabricate a missing leg. The
\ schema carries no per-side ARTIFACT identity (the persist dot deferred cross-artifact
\ linkage to the promotion lane); a report row exposes exactly the fields the schema
\ carries - workload, shape, protocol, baseline, cache state, metric unit, and both
\ numeric-policy witnesses - each participating in the exact key.
\
\ HISTORICAL INVALID PAIR (kept as source evidence, NEVER a competitive result):
\   HISTORICAL-FP32-VS-TF32   Habu blocked FP32 (exact) 380.5 GFLOP/s vs Triton TF32
\   (relative) 1890.5 GFLOP/s. The two sides do NOT share a numeric policy, so the pair
\   is incomparable-by-policy (GFLOPS-COMPARABLE? = 0, the motivating confusion). It stays
\   as SEPARATELY LABELLED source evidence: it still RENDERS (spol=exact|bpol=rel), but the
\   checked importer refuses to persist it as a competitive result - IMPORT-GFLOPS-RESULT
\   throws E-BENCH-INCOMPARABLE. The negative regression (tools/eval-triton-test.f) pins
\   that it can never load as a competitive result.
\
\ Fail closed: importing a non-comparable pair as a competitive result is the named throw
\ E-BENCH-INCOMPARABLE. eval-triton owns -5321 (report owns -5257..-5258, store -5310..-5320).
\ maki -> habu only; this tool consumes the public BENCH surface, zero TRUSTED / set-check.

require lib/prelude.f
require maki/competitive-store.f    \ public typed writers/readers + the sealed store

-5321 constant E-BENCH-INCOMPARABLE   \ a non-comparable pair imported as a competitive result

package EVAL-TRITON
public

\ ---- shipped competitive results (real device numbers, milli-units) ----------
: SAXPY-FP32 ( -- BENCH:comparison-gbps )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:EXACT
   64209 BENCH:>GBPS 63000 BENCH:>GBPS BENCH:COMPARE-GBPS ;
: HABU-MMM-TF32 ( -- BENCH:comparison-gflops )
   BENCH-WORKLOAD:GEMM BENCH-SHAPE:SQ2048 BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:RELATIVE NPOL-DOM:RELATIVE
   884889 BENCH:>GFLOPS 1890500 BENCH:>GFLOPS BENCH:COMPARE-GFLOPS ;

\ ---- the historical invalid pair: labelled SOURCE EVIDENCE, not a result ------
: HISTORICAL-FP32-VS-TF32 ( -- BENCH:comparison-gflops )
   BENCH-WORKLOAD:GEMM BENCH-SHAPE:SQ2048 BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:WARM NPOL-DOM:EXACT NPOL-DOM:RELATIVE
   380501 BENCH:>GFLOPS 1890500 BENCH:>GFLOPS BENCH:COMPARE-GFLOPS ;

\ ---- comparability at the migration layer (bool, never a bare product) --------
: SAXPY-COMPARABLE?      ( -- bool )  SAXPY-FP32              BENCH:GBPS-COMPARABLE? ;
: MMM-COMPARABLE?        ( -- bool )  HABU-MMM-TF32           BENCH:GFLOPS-COMPARABLE? ;
: HISTORICAL-COMPARABLE? ( -- bool )  HISTORICAL-FP32-VS-TF32 BENCH:GFLOPS-COMPARABLE? ;

\ ---- the checked importer: a competitive RESULT must be COMPARABLE? ------------
: IMPORT-GBPS-RESULT ( BENCH:comparison-gbps -- )
   dup BENCH:GBPS-COMPARABLE? 0= if E-BENCH-INCOMPARABLE throw then
   BENCH:BENCH-PUT-GBPS ;
: IMPORT-GFLOPS-RESULT ( BENCH:comparison-gflops -- )
   dup BENCH:GFLOPS-COMPARABLE? 0= if E-BENCH-INCOMPARABLE throw then
   BENCH:BENCH-PUT-GFLOPS ;

\ ---- migrate the shipped evidence: persist both comparable results ------------
: MIGRATE ( -- )
   BENCH:BENCH-STORE-RESET
   SAXPY-FP32    IMPORT-GBPS-RESULT
   HABU-MMM-TF32 IMPORT-GFLOPS-RESULT ;

\ ---- report rows (the canonical render IS the byte-stable report) -------------
: SAXPY-REPORT$      ( -- ptr u8 n )  SAXPY-FP32              BENCH:RENDER-GBPS ;
: MMM-REPORT$        ( -- ptr u8 n )  HABU-MMM-TF32           BENCH:RENDER-GFLOPS ;
: HISTORICAL-REPORT$ ( -- ptr u8 n )  HISTORICAL-FP32-VS-TF32 BENCH:RENDER-GFLOPS ;

\ ---- exact-key lookup over the sealed store (changed key => lookup miss) -------
: RESULT-FOUND? ( ptr u8 n -- bool )   \ true iff a row is stored under this exact key
   BENCH:BENCH-GET {: a:ptr u:n f:bool :} f ;
: SAXPY-FOUND? ( -- bool )  SAXPY-FP32    BENCH:RENDER-GBPS   RESULT-FOUND? ;
: MMM-FOUND?   ( -- bool )  HABU-MMM-TF32 BENCH:RENDER-GFLOPS RESULT-FOUND? ;
\ the SAME saxpy row with cache=cold: one exact-key field changed => a DIFFERENT key.
: SAXPY-COLD-FOUND? ( -- bool )
   BENCH-WORKLOAD:SAXPY BENCH-SHAPE:N1M BENCH-PROTOCOL:CUDA-EVENTS BENCH-BASELINE:TRITON
   BENCH-CACHE--STATE:COLD NPOL-DOM:EXACT NPOL-DOM:EXACT
   64209 BENCH:>GBPS 63000 BENCH:>GBPS BENCH:COMPARE-GBPS BENCH:RENDER-GBPS RESULT-FOUND? ;

\ ---- replay a migrated result to the persisted row (get -> decode -> re-encode) -
: RELOAD-SAXPY$ ( -- ptr u8 n )
   SAXPY-FP32    BENCH:RENDER-GBPS   BENCH:BENCH-GET drop BENCH:BENCH-DECODE-GBPS   BENCH:BENCH-ENCODE-GBPS ;
: RELOAD-MMM$ ( -- ptr u8 n )
   HABU-MMM-TF32 BENCH:RENDER-GFLOPS BENCH:BENCH-GET drop BENCH:BENCH-DECODE-GFLOPS BENCH:BENCH-ENCODE-GFLOPS ;

;package
