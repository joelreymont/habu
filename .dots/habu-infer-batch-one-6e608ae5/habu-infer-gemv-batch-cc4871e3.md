---
title: "Infer GEMV: batch-one benchmark"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T10:07:41.801852+02:00"
blocks:
  - habu-infer-gemv-real-db86a0b7
  - habu-infer-m0-benchmark-67ece165
---

Why this exists:
The batch-one kernel is useful only if it improves the real projection sites under decode-sized workloads.

Required result:
Measure every supported site against the BF16 path under the M0 protocol, including bytes read, launch count, median and 95th-percentile latency, and achieved bandwidth.

Done when:
Correctness is checked before timing; canonical records cover every site and identify regressions; no throughput claim is extrapolated beyond measured shapes.

Expected touch points: canonical benchmark records and the batch-one schedule table.
Smallest check: benchmark-schema validation and reducer replay.
Prerequisites: real-model batch-one integration and M0 benchmark reducer.
Owned result: batch-one GEMV measurement only.
Claim: unassigned.
