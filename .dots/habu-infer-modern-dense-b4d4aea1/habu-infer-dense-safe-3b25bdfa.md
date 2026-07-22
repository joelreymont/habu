---
title: "Infer dense: safe memory-boundary gate"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:41:52.468766+02:00"
blocks:
  - habu-infer-dense-full-14833530
  - habu-infer-alloc-measured-3256cc56
  - habu-infer-kv-exact-a989783c
---

Why this exists:
M5 requires safe startup and controlled rejection near the unified-memory boundary for the pinned model.

Required result:
feed packed weight, workspace, KV geometry, and system reserve classes into one capacity plan before mapping the model.

Done when:
exactly fitting declared context/concurrency succeeds; one-page-over and insufficient reserve reject with an explanation before model load; no transient full-model duplication occurs.

Expected touch points: allocation planner module/test and startup integration.
Smallest check: focused capacity boundary tests plus live headroom probe.
Prerequisites: full BF16 continuation parity, allocation-class result table, exact KV metrics.
Owned result: pinned-model capacity admission only.
Claim: unassigned.
