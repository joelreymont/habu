---
title: "Infer prefill: context-regime benchmark"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T09:43:30.750394+02:00"
blocks:
  - habu-infer-prefill-scheduler-57338337
  - habu-infer-m0-benchmark-c50501b7
  - habu-infer-m0-benchmark-67ece165
---

Why this exists:
M7 requires prefill throughput and time-to-first-token evidence at 1K, 4K, 16K, and longer supported prompts.

Required result:
run unchunked and scheduler-chunked paths under the M0 schema, including decode-stall impact.

Done when:
correctness gate precedes timing; canonical median/p95 and throughput records exist for each regime; selected chunk policy cites data.

Expected touch points: canonical benchmark records and policy row.
Smallest check: M0 schema/reducer.
Prerequisites: scheduler chunk budget and M0 runner.
Owned result: prefill measurement only.
Claim: unassigned.
