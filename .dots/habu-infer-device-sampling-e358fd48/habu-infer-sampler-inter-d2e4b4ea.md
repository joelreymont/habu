---
title: "Infer sampler: inter-token latency gate"
status: open
priority: 2
issue-type: task
created-at: "2026-07-22T09:43:30.715844+02:00"
blocks:
  - habu-infer-sampler-deterministic-15d6915b
---

Why this exists:
device sampling is useful only if synchronization plus kernel cost improves or preserves the real engine's inter-token latency.

Required result:
benchmark host and device dispatch inside the single-sequence engine on the pinned model.

Done when:
canonical records include synchronization cost and median/p95; device path becomes default only when the measured gate passes.

Expected touch points: canonical benchmark record and schedule policy row.
Smallest check: M0 schema/reducer and engine correctness parity.
Prerequisites: deterministic device dispatch and steady-state engine record.
Owned result: sampler performance decision only.
Claim: unassigned.
