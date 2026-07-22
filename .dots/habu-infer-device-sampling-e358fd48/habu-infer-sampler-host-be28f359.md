---
title: "Infer sampler: host critical-path baseline"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.686934+02:00"
blocks:
  - habu-infer-m0-schema-27e8ca5d
---

Why this exists:
the device sampler needs an honest latency target because host sampling blocks the next token for one sequence.

Required result:
measure the canonical host sampler for greedy, temperature, top-k, and top-p over the pinned vocabulary sizes under the M0 schema.

Done when:
warm median/p95 and CPU time are recorded with fixed logits and seeds; no overlap claim is made.

Expected touch points: sampler benchmark and canonical result.
Smallest check: result schema and reducer replay.
Prerequisites: M0 schema and landed host sampler.
Owned result: host sampler timing only.
Claim: unassigned.
