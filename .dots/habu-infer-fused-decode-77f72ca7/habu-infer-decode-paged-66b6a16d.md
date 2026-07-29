---
title: "Infer decode: paged real-model parity"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.390536+02:00"
blocks:
  - habu-infer-decode-real-c0fcdcf4
  - habu-infer-kv-snapshot-1cdc055a
---

Why this exists:
M3 is not complete until page indirection is invisible during a real continuation.

Required result:
run the selected paged kernel over the GPT-2 oracle cache with multiple scatterings and prefix-sharing layouts.

Done when:
attention outputs agree with the contiguous kernel and at least 64 greedy token identifiers match for every supported scattering; run twice is identical.

Expected touch points: paged real-model integration test and fixtures.
Smallest check: correctness-only GB10 64-token parity run.
Prerequisites: select page transfer path, real-step contiguous parity, paged KV snapshot publication.
Owned result: paged real-model acceptance only.
Claim: unassigned.
