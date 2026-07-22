---
title: "Infer: continuous batching scheduler"
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T15:58:37.109397+02:00"
blocks:
  - habu-infer-scheduler-throughput-a04ceb9a
---

This is the continuous-batching campaign record. Do not dispatch it as implementation work. Its leaves own request states, both admission profiles, FIFO ordering, token-boundary batch assembly, cancellation, bounded prefill service, wait and reject reasons, churn invariants, and the final throughput and latency gate. The campaign closes when the integrated scheduler gate lands.
