---
title: Compute Qwen BF16 linear
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:42:53.994789+02:00"
blocks:
  - habu-infer-dense-host-4c9152ad
  - habu-own-qwen-device-f2a8083c
  - habu-open-qwen-device-0db2dea3
  - habu-upload-qwen-weights-ae774802
---

Why: every Qwen projection uses BF16 input and weights, but the retained matrix emitter is F32-only and no production operation owns the pinned weight orientation, bias order, or BF16 output boundary.

Result: package DEVRT owns ADD-QWEN-LINEAR ( qbuild -- add-result ), which compiles one exact function, fills only the empty LINEAR slot, and returns added(qbuild) or refused(qbuild,module-error). Its package-private builder check calls that stored slot with a QWENTENSOR weight role, optional catalog-required bias role, bounded BF16 input rows, and bounded BF16 output rows; completed LOGITS later calls the same slot. The function resolves addresses only through qbuild's private weight lookup and computes input times weight-transpose at exact catalog dimensions. Unbiased projections round once to BF16 at output. Q/K/V bias enters the accumulator before that round. Orientation, bias, and extents come only from QWENTENSOR and MDLCFG.

Add no public function handle, generic installer, generic tensor graph, runtime matrix selector, duplicated role table, transposed weight copy, F32 activation mode, per-call allocation, host fallback, plugin, or alternate bias order. Owner: sole LINEAR-slot transition, exact function, and build-time owning check only. Production red: qbuild has no legal linear installation and Qwen projections have no BF16 device operation. Acceptance: duplicate install rejects with qbuild unchanged; every pinned sparse BF16 word is bit-equal; every element of each named complete Q/K/V/O/gate/up/down/head BF16 output differs by at most one adjacent BF16 representable value; NaN or Inf mismatch fails; post-round bias and weight-orientation mutations fail; wrong role, bias, dtype, shape, generation, or short output rejects before launch; repeated calls reuse storage. Aggregate sums and BF16-to-FP32 drift cannot satisfy acceptance. Smallest owning check: focused GB10 Qwen BF16 linear parity through DEVRT. Claim: unassigned.
