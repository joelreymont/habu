---
title: Map Qwen paged attention rows
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T22:01:58.496422+02:00"
blocks:
  - habu-infer-decode-vector-e5ac69b3
  - habu-add-qwen-model-bf23d2ff
  - habu-open-qwen-device-0db2dea3
  - habu-infer-dense-host-4c9152ad
---

Why: the shared paged recurrence needs one exact Qwen row mapping rather than a generic attention selector. Interface: package DEVRT owns ADD-QWEN-PAGED ( qbuild -- add-result ), which extends DECODEGEOM and DECODE-CG with the exact qwen2 row, fills only the empty PAGED slot, and returns added(qbuild) or refused(qbuild,module-error). Its package-private builder check calls that stored slot; completed LOGITS later calls the same slot. The row maps 28 query heads by integer division to 4 key/value heads, uses head size 128, and stores BF16 K and V in the sole KV layout. Addressing consumes the immutable KV descriptor and authenticated layer, row, and token; accumulation and online softmax are FP32. Owner: sole PAGED-slot transition plus Qwen geometry and BF16 support in the existing recurrence only. Production red: qbuild has no legal paged-function installation and the retained recurrence supports only GPT-2 F32 rows. Acceptance: duplicate install rejects with qbuild unchanged; every pinned sparse BF16 word is bit-equal; every element of each named complete attention BF16 output differs by at most one adjacent BF16 representable value; NaN or Inf mismatch fails; 28:4 rows at first token, page boundary, scattered pages, copy-on-write divergence, and full context match the oracle; wrong mapping, dtype, or extent rejects before launch; GPT-2 stays exact. Aggregate sums and BF16-to-FP32 drift cannot satisfy acceptance. Forbidden: public function handle, generic installer, second cache, descriptor, snapshot, mask authority, prefix policy, model callback, generic attention dialect, or separate recurrence. Smallest owning check: focused GB10 DECODE-CG Qwen row parity. Claim: unassigned.
