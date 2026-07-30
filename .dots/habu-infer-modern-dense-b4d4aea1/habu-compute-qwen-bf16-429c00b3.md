---
title: Compute Qwen BF16 SwiGLU
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:42:54.103949+02:00"
blocks:
  - habu-infer-dense-host-4c9152ad
  - habu-open-qwen-device-0db2dea3
---

Why: the retained SwiGLU emitter is F32-only, while the pinned Qwen path has two load-bearing BF16 rounding boundaries.

Result: package DEVRT owns ADD-QWEN-SWIGLU ( qbuild -- add-result ), which extends the existing emitter with one Qwen BF16 function, fills only the empty SWIGLU slot, and returns added(qbuild) or refused(qbuild,module-error). Its package-private builder check calls that stored slot; completed LOGITS later calls the same slot. For each BF16 gate value, the function widens to FP32, computes SiLU in FP32, rounds once to BF16, multiplies by the BF16 up value, and rounds the product to BF16 output. A BF16 sigmoid intermediate and an unrounded FP32 SiLU-times-up path are forbidden. Geometry comes from MDLCFG and storage from qbuild.

Add no public function handle, generic installer, second SwiGLU emitter, generic activation selector, host fallback, F32 Qwen mode, per-call allocation, global buffer, or fused MLP block. Owner: sole SWIGLU-slot transition, BF16 function, and build-time owning check only. Production red: qbuild has no legal SwiGLU installation and the F32 kernel cannot reproduce the Qwen MLP boundary. Acceptance: duplicate install rejects with qbuild unchanged; every pinned sparse BF16 word is bit-equal; every element of each named complete SwiGLU BF16 output differs by at most one adjacent BF16 representable value; NaN or Inf mismatch fails; BF16-sigmoid and delayed-rounding mutations fail; wrong dtype, extent, or generation rejects before launch; repeated calls reuse storage. Aggregate sums and BF16-to-FP32 drift cannot satisfy acceptance. Smallest owning check: focused GB10 Qwen BF16 SwiGLU parity through DEVRT. Claim: unassigned.
