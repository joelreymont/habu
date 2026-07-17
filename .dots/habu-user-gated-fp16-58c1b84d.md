---
title: "USER-GATED: fp16/bf16 mma numerics policy decision"
status: open
priority: 2
issue-type: task
created-at: "2026-07-17T14:09:15.646955+02:00"
---

From the wave-4 roofline verdict (habu-mma-wave-4-03621e08, 2026-07-17, docs/kernel-principles.md roofline section): the tf32 mma.sync GEMM program is CLOSED at 3026.6 GFLOP/s = 40.2% of the 7520 dense-TF32 peak = 1.60x Triton, and the ONLY remaining instruction-level lever is the denser mma.sync.m16n8k16 fp16/bf16 shape (2x tensor throughput class) - which CHANGES THE NUMERICS CONTRACT vs the current tf32-for-f32 eval policy (tf32: 10-bit mantissa on inputs, f32 accumulate; fp16: 10-bit mantissa half storage; bf16: 8-bit mantissa, f32 range). USER DECISION REQUIRED: whether any Model CAD / maki eval workloads may run reduced-precision GEMM (bf16/fp16 with f32 accumulate) where the accuracy budget allows, or the tf32 contract stays universal. If approved for some tier: mint the implementation program (fragment proofs first per the established discipline, new golden tolerance policy, per-op precision tagging in the schedule/autotuner). If declined: the GEMM perf program stays closed at the tf32 roof attained. No implementation before the decision. Ownership: numerics policy (user-gated).
