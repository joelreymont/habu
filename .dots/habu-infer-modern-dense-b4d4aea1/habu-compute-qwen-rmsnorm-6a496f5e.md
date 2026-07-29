---
title: Compute Qwen RMSNorm
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:01:28.053679+02:00"
blocks:
  - habu-infer-dense-host-4c9152ad
  - habu-open-qwen-device-0db2dea3
  - habu-upload-qwen-weights-ae774802
---

Why: the landed standalone RMSNorm emitter is not installed in the Qwen runtime and Qwen requires the exact Transformers 4.43.1 BF16 order. Interface: package DEVRT owns ADD-QWEN-RMSNORM ( qbuild -- add-result ), which compiles the exact function, fills only the empty RMSNORM slot, and returns added(qbuild) or refused(qbuild,module-error). A package-private builder check calls that stored slot with one device row, QWENTENSOR weight role, hidden size, and runtime epsilon; the completed LOGITS path later calls the same slot. It resolves the role only through qbuild's private weight lookup, widens activations to FP32, computes square, mean, epsilon addition, and reciprocal square root in FP32, multiplies by that reciprocal, rounds to BF16, then performs the BF16 weight multiply and writes BF16 output. Owner: sole RMSNORM-slot transition, exact function, and build-time owning check only. Production red: qbuild has no exact RMSNORM mutation or pinned cast-order parity. Acceptance: duplicate install rejects with qbuild unchanged; every pinned sparse BF16 word is bit-equal; every element of the named complete BF16 output differs by at most one adjacent BF16 representable value; NaN or Inf mismatch fails; moving the cast after weight multiplication fails; wrong role, geometry, dtype, generation, or launch preserves qbuild. Aggregate sums and BF16-to-FP32 drift cannot satisfy acceptance. Forbidden: public function handle, generic installer, epsilon constant, host fallback, per-call allocation, second emitter, generic normalization selector, or fused block. Smallest owning check: bin/hb --load maki/infer/qwen-rmsnorm-test.f on DGX Spark. Claim: unassigned.
