---
title: "Infer GEMM: small-batch benchmark"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T10:07:42.529521+02:00"
blocks:
  - habu-infer-gemm-projection-a7acb2e1
  - habu-infer-m0-benchmark-67ece165
---

Why this exists:
The small-batch path needs measured crossover data rather than an assumed tensor-core win.

Required result:
Measure every supported batch and projection shape against the BF16 and batch-one paths under the M0 protocol.

Done when:
Canonical records include correctness, batch, shape, launch count, median and 95th-percentile latency, throughput, and utilization; the schedule table contains only measured rows.

Expected touch points: canonical benchmark records and the small-batch schedule table.
Smallest check: benchmark-schema validation and reducer replay.
Prerequisites: projection epilogue integration and M0 benchmark reducer.
Owned result: small-batch GEMM measurement only.
Claim: unassigned.
