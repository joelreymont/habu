---
title: Add CUDA-event GPU profiler harness
status: closed
priority: 1
issue-type: task
created-at: "2026-07-01T19:05:25.582115+02:00"
closed-at: "2026-07-01T19:11:52.766338+02:00"
close-reason: "completed: CUDA-event GPU timing added to tools/ptx/bench.f, bandwidth/fusion rows switched to gpu_elapsed_ns, docs and parent dot updated, and focused validation passed (bench/profile tests, typed-local diff lint, lint-libs-ptx-tool, lint-tools)."
---

User caught that tools/ptx/bench.f only timed host launch loops. Fix: add CUDA
Driver event create/record/synchronize/elapsed timing in tools/ptx/bench.f, keep
host timing separate, switch tools/ptx/bandwidth-lib.f and fusion rows to device
elapsed time, update docs/kernel-principles.md and the parent dot with Orin
GPU-event scalar/v4/fusion output, and verify with tools/ptx/bench-test.f plus
Orin cubin benchmark runs. Why: kernel optimization evidence must be generic and
device-timed, not launch-timed.

Proof rows to preserve:

- `tools/ptx/bandwidth.f` on scalar SAXPY: `gpu_elapsed_ns=58709278`,
  `GB/s_x1000=42865`.
- `tools/ptx/bandwidth-v4.f` on v4 SAXPY: `gpu_elapsed_ns=39193473`,
  `GB/s_x1000=64209`.
- `tools/ptx/fusion-compare.f`: unfused v4 SAXPY+v4 RELU
  `gpu_elapsed_ns_sum=66268672`, fused v4 RELU `gpu_elapsed_ns=39208705`,
  `fusion_elapsed_ratio_x1000=1690`.
