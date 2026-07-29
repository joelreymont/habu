---
title: Execute Qwen QKV stage
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:02:28.610564+02:00"
blocks:
  - habu-upload-qwen-weights-ae774802
  - habu-compute-qwen-rmsnorm-6a496f5e
  - habu-compute-qwen-split-d1dcb69b
  - habu-compute-qwen-bf16-ed55a127
  - habu-map-qwen-paged-426d1d76
  - habu-infer-kv-atomic-cdfb00cb
  - habu-infer-dense-host-4c9152ad
---

Why: Qwen attention cannot run until one checked stage composes input RMSNorm, biased Q/K/V projections, split-half RoPE, and provisional K/V writes. Interface: package DEVRT owns ADD-QWEN-QKV ( qbuild -- add-result ), which installs the exact composition over the already-filled RMSNORM, LINEAR, and ROPE slots, fills only QKV, and returns added(qbuild) or refused(qbuild,module-error). Its package-private builder check takes qbuild, one immutable provisional KV descriptor, authenticated layer and positions, and bounded BF16 device activations. It resolves roles through qbuild, invokes the exact slots so bias enters before one BF16 output round, and writes K/V only to descriptor-authorized coordinates. It never retains, commits, aborts, or mutates a KV owner; RUN-ROWS alone pairs the later session launch with KV:ready. Owner: sole QKV-slot transition, composition, and build-time owning check only. Production red: qbuild has no exact QKV transition or real descriptor path. Acceptance: duplicate install rejects with qbuild unchanged; every pinned sparse BF16 word is bit-equal; every element of each named complete Q/K/V and post-RoPE BF16 output differs by at most one adjacent BF16 representable value; NaN or Inf mismatch fails; first and last layer plus page-edge positions pass; bias-order and half-mapping mutations fail; launch failure leaves committed KV unchanged. Aggregate sums and BF16-to-FP32 drift cannot satisfy acceptance. Forbidden: public function handle, generic installer, host fallback, FP32 activation shortcut, second QKV layout, raw pointer, KV owner mutation, per-call allocation, fused block, generic graph, or partial commit. Smallest owning check: focused GB10 Qwen QKV/KV stage parity. Claim: unassigned.
