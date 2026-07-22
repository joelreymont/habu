---
title: "Infer scheduler: throughput and p95 gate"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T09:43:30.851298+02:00"
blocks:
  - habu-infer-scheduler-churn-896201fe
  - habu-infer-batch-decode-4a35fb22
  - habu-infer-prefill-ctx-1dc63ffe
  - habu-infer-launch-implement-f88881d3
---

Why this exists:
continuous batching is incomplete without aggregate throughput and p95 inter-token evidence under supported concurrency.

Required result:
benchmark mixed arrivals and prompts for both admission profiles, recording queue waits, prefill stalls, throughput, and p95.

Done when:
canonical results cover concurrency one through the supported bound and prove the documented backpressure behavior; correctness and churn gates pass first.

Expected touch points: canonical benchmark records and supported-profile table.
Smallest check: M0 schema/reducer.
Prerequisites: churn property test, batched decode benchmark, prefill benchmark, replay mechanism.
Owned result: scheduler performance acceptance only.
Claim: unassigned.
