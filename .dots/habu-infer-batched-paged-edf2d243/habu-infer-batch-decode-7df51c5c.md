---
title: "Infer batch decode: real-model parity"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.773208+02:00"
blocks:
  - habu-infer-batch-decode-57ee14e3
  - habu-infer-dense-full-14833530
---

Why this exists:
synthetic ragged checks do not prove multiple real prompts with divergent lengths.

Required result:
run a bounded set of pinned-model prompts together and compare every row and greedy token with independent single-sequence execution.

Done when:
at least 64 generated steps match per row, including early completion and prefix sharing; run twice is identical.

Expected touch points: real-model batched fixture/test.
Smallest check: correctness-only GB10 continuation parity.
Prerequisites: completion masking and modern full BF16 parity.
Owned result: batched real-model correctness only.
Claim: unassigned.
