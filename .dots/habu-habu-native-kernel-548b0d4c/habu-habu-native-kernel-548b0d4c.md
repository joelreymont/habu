---
title: Habu-native kernel benchmark and profile workflow
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-01T18:24:40.268057+02:00\""
---

User requirement 2026-07-01: kernel work must add benchmarking/profiling and use
it extensively. Root cause: existing perf evidence is scattered around
tools/ptx/bandwidth.f and docs, while M9 remains open and new maki/PTX changes
can land without a reusable benchmark/profile row. Fix: build/extend checked
Habu benchmark/profiling support around device kernels, record GB/s/GFLOP/s/%roof
and launch configuration, add focused tests/docs, and require new maki/PTX kernel
changes to run the relevant benchmark/profile slice. Why: kernel decisions must
be evidence-driven per docs/kernel-principles.md, not just correctness-gated.

Checkpoint 2026-07-01:

- Added checked Habu profile math in `tools/ptx/profile.f` and wired it into the PTX toolchain suite.
- Split scalar/v4 SAXPY benchmark runners through `tools/ptx/bandwidth-lib.f`.
- Used the runner on Orin after checked emit + `ptxas`: scalar SAXPY 42.903 GB/s, v4 SAXPY 63.783 GB/s.
- RCA from the v4 profile attempt fixed stale elementwise codegen register declarations in `lib/ptx/cg.f`.

Checkpoint 2026-07-01 genericization:

- Added `tools/ptx/bench.f` as the generic CUDA Driver benchmark/profile layer:
  cubin/kernel/label config, grid/block/iters/work-items, arbitrary param
  offsets, CUDA alloc/memcpy/memset/free helpers, host launch timing, and
  CUDA-event GPU elapsed timing.
- Refactored `tools/ptx/bandwidth-lib.f` so SAXPY is just one workload-specific
  setup over the generic harness.
- Added `tools/ptx/fusion-compare.f`; corrected Orin CUDA-event rows show scalar
  SAXPY 42.865 GB/s, v4 SAXPY 64.209 GB/s, unfused v4 SAXPY+v4 RELU summing to
  66.269 ms / 200 iters, and fused v4 RELU at 39.209 ms / 200 iters
  (`fusion_elapsed_ratio_x1000=1690`).

Remaining: make each M9/GEMM/attention optimization carry its own profile row
and keep perf-regression gating dotted there.

## Parked 2026-07-13 (session limit, BLOCK on review)
Worker (kbench) committed bed31988 in .jj-ws/fable-kbench: perf-row registry
(tools/ptx/perf-rows.tsv + perf-registry.f), perf-compare.f + perf-regress.f,
kernel-perf-lint{,-core,-test}.f, wired into TEST:SUITE ptx-toolchain.
Destruction review verdict BLOCK: the new tests are wired only into the SPAWNED
slice, not the inprocess duplicate list GSI-LINT-LIBS-PTX-TOOL
(test/gate-stdlib-inline-lib.f), so test/run.f never runs them (orphan-suite
class). Plus cheap correctness fixes: ROW-CELL bounds-checks capacity not row
count; DATE-OK? accepts 2026-99-99; bad TSV row fails as bare -7300 with no line
context; dead PERF:LINE@. Fix round dispatched then died at the session limit
with nothing committed. DO NOT MERGE as-is. Resume: add the 7 entries to
GSI-LINT-LIBS-PTX-TOOL + the correctness fixes, rerun test/run.f. Claim released.
Advisories (dot separately): hunk-aware diff parsing, waiver ratchet, watch-set
extension for lib/ptx/tile*.f/opt*.f/ir.f.
