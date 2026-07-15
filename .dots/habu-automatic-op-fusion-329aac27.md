---
title: Automatic op-fusion pass (register-resident, the bandwidth win)
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T15:43:15.234314+02:00"
---

RECONCILED 2026-07-15 against the landed lowering campaign; the original build
items are DONE under different names: (1) op-graph IR = the typed MIR
(maki/mir*.f); (2) fusibility analysis = maki/fusion-plan.f (FP-BASE-FUSE?
class-pair legality + backend capability table, reductions/reshapes as
barriers, movement dissolution, FP-FUSE-OFF! ablation); (3) one-kernel-per-
region register-resident codegen = maki/lower-ew.f LEW-CHAIN (multi-op
elementwise chains, intermediates in registers, movements folded), plus the
matmul epilogue path in lower-mm/lower-model. Kernel-level device proof
already landed: tools/ptx/fusion-compare.f runs FUSED-RELU-V4 vs unfused
SAXPY-V4+RELU-V4 on zed (real compare, exit 0). Host-side plan ablation is
maki/ablate-fusion-test.f; its LATENCY leg was explicitly deferred as device
work.

REMAINING SCOPE (device, zed): the end-to-end proof through the AUTOMATIC
pipeline - take a multi-op elementwise chain model (e.g. Add->Mul->Relu, same
shape), lower it twice on-device: fusion ON (one region, one kernel) vs
FP-FUSE-OFF! (per-op regions, N kernels with global round-trips), assert both
device-correct vs the same golden, and record the measured bandwidth/latency
ratio as a perf-registry row pair (orin-nx-25w). Acceptance: fused runs 1
kernel vs N, both correct, fused wins on effective bandwidth (record the
actual ratio; the design expectation is ~Nx fewer global round-trips), rows
committed with the magnitude-independent corruption-probe discipline. Optional
stretch (separate row, only if cheap): a per-op Triton elementwise baseline
comparison via the typed BENCH import path. Files: a maki/ablate-fusion
device test (or extension of lower-model-device harness), tools/ptx perf
registry rows, MODEL-CAD-V2-PLAN.md evidence note. Verify: on-device run
green, off-device SKIP honest, maki/test.f, perf-registry lint. Ownership:
maki lowering + ptx perf evidence.
