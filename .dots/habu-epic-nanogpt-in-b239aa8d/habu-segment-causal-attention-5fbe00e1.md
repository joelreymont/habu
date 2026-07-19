---
title: Segment/causal attention op + adjoint
status: closed
priority: 2
issue-type: task
created-at: "\"2026-07-18T17:36:01.935952+02:00\""
closed-at: "2026-07-19T07:37:45.336765+02:00"
blocks:
  - habu-causal-attention-mask-1ced9cbd
---

Segment/causal self-attention op-kind + host reference + adjoint: contracts block-diagonally per sequence (rows = B*T, B outermost, block [b*T,b*T+T)) and causally within a block; block width T + triangular constraint in the attrs cell (model-ir.f:116); never materializes a (B*T)x(B*T) score. Composes MM-NT (attention.f:18), masked SM-FWD (softmax.f:26 + causal-mask dot), MATMUL (matmul.f:22) per block; executor arm (executor.f:343); adjoint = per-block BW-STEP-MATMUL (backward.f:240) + unchanged per-row BW-STEP-SOFTMAX. Acceptance in construction terms: multi-sequence numeric gradcheck vs single-sequence ATTN-FWD/ATTN-BWD (attention.f:37,71) per block, zero cross-sequence coupling. Static checker reject is the cross-seq-reject dot, not this one. Full contract: docs/batch-sequence-design.md section 5 BTC-1.
