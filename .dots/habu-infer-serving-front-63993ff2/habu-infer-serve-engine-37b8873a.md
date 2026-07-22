---
title: "Infer serve: engine event stream"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:43.361181+02:00"
blocks:
  - habu-infer-serve-completion-987f5b4b
  - habu-infer-scheduler-churn-896201fe
---

Why this exists:
Transports need one ordered stream of admission, token, completion, error, and cancellation events without reading engine internals.

Required result:
Define the request-scoped event stream and its ownership rules between the continuous scheduler and one client lease.

Done when:
Every accepted request produces one ordered terminal stream; token identifiers and text fragments agree; duplicate terminal events, events after cancellation, and cross-request delivery reject.

Expected touch points: serving event protocol and focused deterministic traces.
Smallest check: the focused event-ordering test.
Prerequisites: completion request schema and continuous scheduler.
Owned result: transport-independent engine event stream only.
Claim: unassigned.
