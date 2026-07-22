---
title: "Infer decode: vector-load paged kernel"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.368193+02:00"
blocks:
  - habu-infer-decode-paged-f188c9e8
  - habu-infer-decode-contiguous-e09bacf2
---

Why this exists:
the paged correctness baseline needs a simple transfer path before specialized TMA or asynchronous candidates.

Required result:
extend the contiguous recurrence with vectorized global loads through the paged gather iterator.

Done when:
the same logical cache in contiguous and randomly scattered pages produces equivalent outputs for short, medium, and long contexts; prefix-shared rows read each physical page correctly.

Expected touch points: new lib/ptx/cg-decode-paged-vector.f, focused device test, perf-watch and FILEMAP rows.
Smallest check: correctness-only GB10 parity run.
Prerequisites: paged gather iterator, contiguous device kernel.
Owned result: vector-load paged kernel only.
Claim: unassigned.
