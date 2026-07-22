---
title: "Infer scheduler: bounded prefill service"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.834815+02:00"
blocks:
  - habu-infer-scheduler-token-3490da89
  - habu-infer-prefill-scheduler-57338337
---

Why this exists:
long prefills can starve ready decodes unless the scheduler enforces the landed chunk budget.

Required result:
select waiting/prefilling work under FIFO and execute at most the declared prefill token budget between decode steps.

Done when:
traces prove ready decode stall is bounded, prefill makes deterministic progress, and cancellation between chunks cleans up.

Expected touch points: scheduler prefill integration/test.
Smallest check: focused mixed prefill/decode traces.
Prerequisites: token-boundary assembly and prefill chunk budget.
Owned result: prefill scheduling only.
Claim: unassigned.
