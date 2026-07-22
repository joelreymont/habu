---
title: "Infer KV: measure page-token candidates"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T09:38:16.967922+02:00"
blocks:
  - habu-infer-kv-exact-a989783c
  - habu-infer-kv-immutable-1ec13a88
  - habu-infer-m0-schema-27e8ca5d
---

Why this exists:
the current 16-token page default is unmeasured and may trade tail waste against gather efficiency poorly on GB10.

Required result:
use the M0 benchmark schema to measure a small declared candidate set on the pinned model across short, medium, and long contexts, recording tail waste, metadata traffic, and decode latency.

Done when:
committed results identify the measured default and supported alternatives without hard-coding a winner before data; the allocator remains parameterized.

Expected touch points: page-geometry benchmark, canonical result record, and the inference-plan result note.
Smallest check: run the focused Habu benchmark reducer and validate its schema.
Prerequisites: exact physical page metrics, immutable device snapshot, M0 schema.
Owned result: page-size experiment and result only; no allocator redesign.
Claim: unassigned.
