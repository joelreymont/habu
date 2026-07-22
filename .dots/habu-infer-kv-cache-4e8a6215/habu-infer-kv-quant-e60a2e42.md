---
title: "Infer KV quant: attention read path"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:44.942727+02:00"
blocks:
  - habu-infer-kv-quant-7ce446e0
  - habu-infer-decode-paged-66b6a16d
---

Why this exists:
Paged attention must consume compressed cache pages and scales directly rather than reconstructing a full BF16 cache.

Required result:
Add one profile-aware paged attention read path that loads, rescales, and feeds the existing online-softmax recurrence for the supported geometry.

Done when:
Short, medium, long, ragged, and prefix-shared fixtures match the BF16 recurrence within the profile's measured limit; incompatible pages reject before launch.

Expected touch points: paged decode emitter variant and focused device tests.
Smallest check: the correctness-only DGX Spark compressed-cache parity test.
Prerequisites: compressed append conversion and selected paged decode path.
Owned result: compressed-cache attention reads only.
Claim: unassigned.
