---
title: V2 competitive evidence matrix
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-11T12:14:24.933792+02:00\""
---

Problem: MODEL-CAD-V2-PLAN.md:1376-1395 and 1530-1545 require non-cherry-picked exact-key comparisons, but current docs/eval-triton.md rows are manually assembled and do not share one versioned evidence schema. Fix: add the first checked report schema for workload/revision/shape/numeric-policy/target/compiler/cache/protocol plus baseline identity and latency/throughput/bytes/launches/memory/energy fields; migrate one SAXPY and one GEMM row. Acceptance: mismatched numeric domains cannot share a comparison row; cold/warm states are explicit; replay renders byte-stable output. Files: maki/report.f, maki/store.f, docs/eval-triton.md, MODEL-CAD-V2-PLAN.md:1376-1395. Verify: focused report/store tests, maki/test.f, host-lint, filemap-lint.

NOTE 2026-07-18 (run-identity landing 785c4021): this dot also owns the
metric UNITS vocabulary (the run-metric package models population/
direction/aggregation; units deferred pending an owner - decide the
closed vocabulary here where the evidence matrix consumes it).

Claim: agent=compev workspace=.jj-ws/fable-compev (checked evidence schema + SAXPY/GEMM row migration + the metric UNITS closed vocabulary)

PROGRESS 2026-07-18 (compev lane, implementation complete; NOT closed - awaiting
review + merge by the orchestrator):

SCHEMA (maki/competitive-evidence.f, package CEVID, owns -5417..-5421). A per-side
EVIDENCE row is a single-cell handle over a bounded first-slice pool (the
run-metric ring precedent). Identity fields: workload/shape/protocol/baseline/
cache-state (reused BENCH enums), revision + compiler (new closed enums),
numeric-policy (NPOL:dom witness), target (CAD-KIND:target-id via TARGET:SM87).
Metric fields (each a `reading` = present (value,unit) | named BENCH:absence):
latency, throughput, bytes, launches, memory, energy. RENDER emits a versioned
byte-stable cevid/v1 row; RENDER-PAIR emits a cevid-cmp/v1 comparison row.

UNITS VOCABULARY (the run-metric deferred 4th axis, decided here; sealed closed
`unit` enum): ns, ms, gflops, gbps, bytes, count, watts. Derivation from the REAL
corpus (docs/eval-triton.md + tools/ptx/perf-rows.tsv): gflops = GEMM GFLOPS;
gbps = SAXPY GB/s; ns = GEMM gpu_elapsed_ns (GB/s = bytes/elapsed_ns); ms = Triton
time_ms=272.6; bytes = SAXPY ITERS*3*N*4 + GEMM smem 50176 B; count = iters;
watts = orin-nx-25w SoC power budget. joules EXCLUDED (no measured energy in the
corpus - would be an unused unit). Values are integers: rate units + ms are
milli-scaled x1000 (the perf-registry convention: 3026.577 GFLOP/s -> 3026577),
ns/bytes/count/watts natural. Each unit maps to a category (duration/rate/size/
tally/power); a reading whose unit is outside its field category is the named
throw E-CEVID-UNIT. Dated note appended to the run-identity dot
(habu-v2-experiment-run-7c1d1906, closed) recording the vocabulary now exists here.

MIGRATED ROWS (maki/competitive-evidence-test.f byte goldens, exact figures
preserved; falsified non-vacuous by mutating 3026577->3026578 -> TFAIL):
  - GEMM flagship: Habu MMM-WIDE-B-M4-S1 thr=3026577 gflops (3026.577 = 3026.6),
    launch=30, mem=50176 bytes, energy=25 watts, warm, TF32/relative, tgt=sm_87;
    baseline Triton thr=1890500 gflops, lat=272600 ms; ratio 3026577/1890500 =
    1.601x. Pair COMPARABLE? (both relative) and renders byte-stably.
  - SAXPY: Habu SAXPY-V4 thr=64209 gbps vs Triton 63000 gbps, byt=2516582400,
    launch=200, energy=25 watts, warm, FP32/exact; pair COMPARABLE?, byte-stable.
  - ns exercised + all-na default render pinned via NS-ROW (real step-1 ns figure).
docs/eval-triton.md + MODEL-CAD-V2-PLAN.md § 22.10 now point at the schema/test.

NEGATIVE TESTS: mismatched numeric DOMAIN cannot share a comparison row - runtime
BAD-CMP (exact FP32 vs relative TF32) = 0 AND RENDER-PAIR throws
E-CEVID-INCOMPARABLE, each with a resolving positive (both-relative comparable).
STATIC checker rejects (CHECK-QUIET-CANDIDATE! = 0 + resolving -1): EVID:prec-class
in the NPOL:dom slot, raw n in an id slot, a baseline id in the workload slot.
Fail-closed throws pinned: E-CEVID-UNIT (rate reading in the latency slot),
E-CEVID-CAP (over-cap / negative value), E-CEVID-ROW (oversized stable row).
cold/warm alters the rendered key (explicit field, no default).

GATE TABLE (all in .jj-ws/fable-compev, HB_TMP=/tmp/hbtmp-compev, native bin/hb):
  maki/competitive-evidence-test.f      EXIT 0  test: ok
  competitive-report/store + eval-triton EXIT 0  test: ok (siblings unbroken)
  report-test / store-test / numpolicy   EXIT 0  test: ok
  maki/test.f                           EXIT 0  (cevid suite peak ndict 5442/16384)
  test/gate-stdlib.f                    EXIT 0  native lint/stdlib phase PASS
  typed-local-diff-lint (jj diff --git) EXIT 0
  dot-dep-lint                          0 finding(s)
  stale-status-lint                     0 finding(s)
  host-lint                             0 finding(s)
  filemap-lint                          0 finding(s)
  error-code-lint                       0 finding(s)
  trust-lint                            0 finding(s)  (zero new TRUSTED/TRUST)
  refine-lint                           0 finding(s)
  trusted-inventory -- strict           EXIT 0
No src/* or fixpoint-lib changes.

REMAINING (follow-up dots, NOT blockers - all stated acceptance met): a durable
typed store codec for cevid/v1 rows (persist/rehydrate/replay, mirroring
maki/competitive-store.f for bench/v1) is out of this dot's canonical-render-only
scope; and optionally wiring the units enum into run-metric.f MEASURE if/when a
run consumes per-metric units (kept in the matrix schema for now to avoid changing
MEASURE's signature + every caller).
