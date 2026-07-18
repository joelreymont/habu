---
title: Multi-head self-attention sublayer composition
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T15:25:04.133383+02:00"
blocks:
  - habu-spec-word-generating-0729fbea
---

The single-running-value MODEL: DSL CANNOT express an internal Q@K^T: it drains params FIFO and threads ONE running value (cad.f CAP-EMIT-PARAMS, adam-train.f note); ADAM-ATTN sidesteps it by passing Q,Kt,V as SEPARATE inputs, single-head, no mask. attention.f golden is single-head/no-mask/no-batch. Author the FULL sublayer as SPEC: lines: QKV projections from x, head split/merge, scaled scores, causal mask, softmax, A@V, output projection. Deps (noted): causal-attention-mask, batch-sequence-tensor design; hard-blocked on SPEC: word (habu-spec-word-generating).
