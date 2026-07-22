---
title: "Infer scheduler: churn property test"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.846326+02:00"
blocks:
  - habu-infer-scheduler-bounded-53574658
  - habu-infer-scheduler-cancellation-523c6cb8
---

Why this exists:
independent unit tests do not prove long mixed arrival, prefill, decode, completion, cancellation, and failure sequences preserve global ownership.

Required result:
add a deterministic randomized model comparing scheduler and a simple oracle after every step.

Done when:
thousands of mixed steps preserve request uniqueness, queue bounds, allocator accounting, snapshot leases, and terminal cleanup; a seeded mutation proves the property test is load-bearing.

Expected touch points: scheduler property test only.
Smallest check: focused deterministic property run.
Prerequisites: bounded prefill service and scheduler cancellation/failure.
Owned result: scheduler property oracle and test only.
Claim: unassigned.
