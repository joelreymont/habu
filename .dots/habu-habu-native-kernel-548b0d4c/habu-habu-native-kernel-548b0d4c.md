---
title: Habu-native kernel benchmark and profile workflow
status: open
priority: 1
issue-type: task
created-at: "2026-07-01T18:24:40.268057+02:00"
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
