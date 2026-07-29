---
title: "Infer KV quant: capacity accounting"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:44.641264+02:00"
blocks:
  - habu-infer-kv-quant-246e57d2
---

Why this exists:
Admission must include compressed data, scales, metadata, alignment, and tail waste before promising a context limit.

Required result:
Extend the key/value-cache geometry calculation for each supported profile and derive exact bytes per token, bytes per page, metadata bytes, and safe aggregate tokens.

Done when:
Every supported geometry matches explicit byte fixtures; one-byte and one-page overflow reject before allocation; the BF16 profile remains byte-identical to current accounting.

Expected touch points: key/value-cache geometry and focused boundary tests.
Smallest check: the focused exact-byte accounting test.
Prerequisites: key/value-cache quality profile and exact physical page metrics.
Owned result: compressed-cache capacity math only.
Claim: unassigned.
