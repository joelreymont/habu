---
title: "Infer batch decode: completion masking"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.767837+02:00"
blocks:
  - habu-infer-batch-decode-fbb73535
---

Why this exists:
rows completing at different steps must be removed without stale output or KV writes.

Required result:
bind completion masks to descriptor generations and exclude masked rows from output publication and append commit.

Done when:
every completion position in a mixed batch matches separate single-sequence runs; stale and double completion reject; completed rows retain no lease.

Expected touch points: batched decode integration module/test.
Smallest check: focused mixed-completion test.
Prerequisites: ragged kernel.
Owned result: completion masking and row retirement only.
Claim: unassigned.
