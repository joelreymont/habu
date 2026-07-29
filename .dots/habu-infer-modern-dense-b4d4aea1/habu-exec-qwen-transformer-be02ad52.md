---
title: Execute Qwen transformer block
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:02:40.649294+02:00"
blocks:
  - habu-infer-dense-grouped-bc7e6656
  - habu-compute-qwen-bf16-429c00b3
---

Why: paged attention output alone does not prove the exact Qwen decoder layer. Interface: package DEVRT owns ADD-QWEN-BLOCK ( qbuild -- add-result ), which installs the exact composition over the filled QKV, ATTN, RMSNORM, LINEAR, and SWIGLU slots, fills only BLOCK, and returns added(qbuild) or refused(qbuild,module-error). Its package-private builder check takes qbuild, one immutable provisional KV descriptor, authenticated layer and position, and one BF16 device activation row; it invokes the stored operations for attention, gate/up, SwiGLU, down, and the second residual. Every tensor comes from QWENTENSOR and every extent from MDLCFG. It never mutates or retains a KV owner. Owner: sole BLOCK-slot transition, decoder composition, and build-time owning check only. Production red: qbuild has no exact block transition for a full pinned Qwen layer. Acceptance: duplicate install rejects with qbuild unchanged; every pinned sparse BF16 word is bit-equal; every element of each named complete pre-attention, post-attention, and post-MLP BF16 output at layers 0, 13, and 27 differs by at most one adjacent BF16 representable value; NaN or Inf mismatch fails; tensor swaps, forbidden bias, moved rounding, wrong layer, and launch failure leave committed KV unchanged; repeated layers reuse addresses. Aggregate sums and BF16-to-FP32 drift cannot satisfy acceptance. Forbidden: public function handle, generic installer, duplicate O projection, host block, FP32 residual or MLP shortcut, second model representation, generic graph executor, per-layer allocation or compilation, fallback, whole-model unroll, or duplicated primitive. Smallest owning check: focused GB10 real Qwen block parity. Claim: unassigned.
