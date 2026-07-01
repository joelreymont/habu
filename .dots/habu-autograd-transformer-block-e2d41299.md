---
title: "Autograd: transformer-block VJP coverage (matmul/attention/layernorm/GELU/residual/embedding)"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T23:00:02.511319+02:00"
blocks:
  - habu-add-logits-domain-a1489686
  - habu-ptx-m11-attention-fa7b0598
  - habu-ad-thread-saved-36bad526
  - habu-maki-lower-tensor-e6bbca3d
---

File: PLAN.md:432. Gap: GPT-path backward needs tensor-scale VJPs for
matmul, causal attention, LayerNorm, GELU, residual, embedding/gather, and
logits-domain CE, with saved values keyed by op instance instead of global
`SAVED-*` stubs. Fix: add checked VJP entries and generated backward fixtures
that lower through the generic PTX/Maki device path, using scatter-add for
embedding/gather accumulation and save-vs-recompute for attention/LayerNorm.
Verify: CPU numeric gradchecks plus Orin finite-difference gradchecks for each
lowered op, negative fixtures for unsupported control flow/shapes, two nonlinear
op instances, and forward-mutation-before-backward aliasing.
