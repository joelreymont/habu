---
title: Batch GPT-2 paged attention
status: open
priority: 1
issue-type: task
created-at: "2026-07-29T23:22:20.209752+02:00"
blocks:
  - habu-infer-engine-64-02416606
  - habu-infer-batch-decode-a7520e15
  - habu-exec-gpt-2-29a09d1a
  - habu-infer-decode-vector-e5ac69b3
---

Why: dense row indexing is insufficient unless K and V writes plus paged attention use each row's authenticated ragged block table. Result: extend the existing GPT2DEV QKV write and DECODE-CG call to index one through four active row descriptors, writing K and V only at each row's provisional page coordinate and reading that row's committed plus provisional length. Dependencies: vector-paged attention and INFER row descriptor. Owner: GPT-2 multi-row K/V and paged-attention indexing only. Production red: one descriptor row is consumed today. Acceptance: batches one, two, and four with mixed lengths, scattered pages, shared pages, and page edges match independent attention; all three issue the same launch count; zero, five, row swap, and page-table mutations fail before committed state changes. Forbidden: second attention recurrence, contiguous comparison kernel, host loop, completion mask, scheduler state, commit, allocation, or fallback. Smallest owning check: bin/hb --load maki/infer/gpt2-device-attention-batch-test.f on DGX Spark.
