---
title: "Infer KV quant: performance decision"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:45.221571+02:00"
blocks:
  - habu-infer-kv-quant-65dbe12a
  - habu-infer-m0-pinned-17b6e648
---

Why this exists:
Cache compression should ship only where reduced bandwidth or increased capacity outweighs conversion and scale overhead.

Required result:
Measure BF16 and each accepted cache profile over context and concurrency regimes where cache reads dominate, then publish the supported profile and schedule rows.

Done when:
Correctness precedes timing; records include memory, bytes per token, page use, inter-token latency, throughput, and faults; the decision cites only measured regimes.

Expected touch points: canonical benchmark records, profile registry, and decision note.
Smallest check: benchmark reducer replay and decision-rule fixture.
Prerequisites: long-context quality verdict and pinned baseline results.
Owned result: key/value-cache quantization release verdict only.
Claim: unassigned.
