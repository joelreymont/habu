---
title: "Infer batch decode: ragged kernel"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.761923+02:00"
blocks:
  - habu-infer-batch-decode-a7520e15
---

Why this exists:
M3 stage C requires one launch over a bounded ragged batch without a host per-head loop.

Required result:
extend the selected paged recurrence across the batch descriptor with per-row lengths and GQA mapping.

Done when:
each active row matches the single-sequence kernel, masked rows produce no output or state mutation, and unsupported batch/geometry rejects before launch.

Expected touch points: new lib/ptx/cg-decode-batched.f, device test, perf-watch/FILEMAP rows.
Smallest check: correctness-only GB10 parity.
Prerequisites: ragged descriptor and selected page transfer path.
Owned result: batched decode kernel only.
Claim: unassigned.
