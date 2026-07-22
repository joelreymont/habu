---
title: "Infer batch decode: regime benchmark"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T09:43:30.779009+02:00"
blocks:
  - habu-infer-batch-decode-7df51c5c
---

Why this exists:
batch support needs throughput and p95 inter-token evidence across bounded batch and ragged-context regimes.

Required result:
benchmark the supported batch sizes and short/medium/long length mixes under M0.

Done when:
correctness precedes timing; records include active/masked rows, batch, contexts, median/p95, and utilization; no unsupported extrapolation.

Expected touch points: canonical result records and schedule table.
Smallest check: M0 schema/reducer.
Prerequisites: real-model batched parity.
Owned result: batched decode measurement only.
Claim: unassigned.
