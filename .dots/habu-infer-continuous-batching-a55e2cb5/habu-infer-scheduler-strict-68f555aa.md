---
title: "Infer scheduler: strict max reservation"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:43:30.806876+02:00"
blocks:
  - habu-infer-scheduler-req-1ac1dac6
  - habu-infer-kv-declared-a0319bef
---

Why this exists:
the strict profile must reserve declared maximum KV before admission and explain rejection.

Required result:
map request limits to exact KV pages and atomically acquire the reservation.

Done when:
exact fit succeeds, one-page-over rejects, cancellation returns all unused pages, and the reason records requested and available capacity.

Expected touch points: new maki/infer/admission-strict.f, focused test.
Smallest check: focused boundary and churn tests.
Prerequisites: request state machine and KV declared maximum admission.
Owned result: strict admission profile only.
Claim: unassigned.
