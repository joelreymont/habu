---
title: "Infer scheduler: FIFO waiting queue"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.816726+02:00"
blocks:
  - habu-infer-scheduler-req-1ac1dac6
---

Why this exists:
fairness and backpressure are undefined without one explicit queue discipline.

Required result:
implement bounded first-in-first-out waiting with generation-bearing request handles and named queue-full rejection.

Done when:
enqueue/dequeue order is exact through cancellations; stale handles cannot remove a new request; full queue does not mutate state.

Expected touch points: new maki/infer/request-queue.f, focused test.
Smallest check: focused queue property test.
Prerequisites: request state machine.
Owned result: waiting queue and FIFO policy only.
Claim: unassigned.
