---
title: Per-op precision tags and golden tolerance policy
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T12:25:31.904396+02:00"
---

The approved branch of the ratified numerics policy (habu-user-gated-fp16-58c1b84d, Joel 2026-07-19: reduced-precision GEMM allowed where the accuracy budget allows). Two pieces. (1) Per-op precision tagging: a schedule/plan-level attribute naming each GEMM-class op's compute dtype (tf32 default, fp16 opt-in, bf16 when it lands), carried from the authoring surface (the equation/MODEL: layer picks it or inherits a workload default) down to the emitter knob (MMA-DTYPE), so precision is declared per op - never a global flip. (2) Golden tolerance policy: reduced-precision ops cannot use the zero-tolerance integer-fill argument on REAL data - define the documented comparison contract for fp16-compute goldens (error model: input rounding to 10-bit mantissa + f32 accumulation order; state the bound and its derivation in the test header, no bare epsilons). Design the attribute shape to fit the equation op-kind (habu-equation-op-kind-93b0d3ba) so equation ops are taggable from day one.
