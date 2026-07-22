---
title: "Infer scheduler: token-boundary batch assembly"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.823651+02:00"
blocks:
  - habu-infer-scheduler-fifo-88f80a53
  - habu-infer-scheduler-strict-68f555aa
  - habu-infer-scheduler-conservative-80ce81be
  - habu-infer-batch-decode-a7520e15
---

Why this exists:
ready requests must join a bounded decode batch only at a defined token boundary with valid snapshots and leases.

Required result:
build one batch descriptor from decoding requests after retiring the prior step and admitting ready work.

Done when:
deterministic traces match independent request states; no request appears twice; over-bound requests wait; stale snapshot prevents assembly before launch.

Expected touch points: new maki/infer/scheduler-step.f, focused test.
Smallest check: focused step traces.
Prerequisites: FIFO queue, both admission profiles, ragged batch descriptor.
Owned result: decode batch assembly only.
Claim: unassigned.
