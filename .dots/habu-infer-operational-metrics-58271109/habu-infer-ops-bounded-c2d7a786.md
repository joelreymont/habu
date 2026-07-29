---
title: "Infer ops: bounded soak runner"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:45.826648+02:00"
blocks:
  - habu-infer-serve-one-c0c151d2
  - habu-infer-ops-scrape-b40bd2ef
  - habu-infer-m0-adapter-2965b6c9
---

Why this exists:
The eight-hour gate needs a reproducible workload driver that preserves raw failures and continuously checks ownership invariants.

Required result:
Drive the declared mixed workload for a configured duration, sample metrics and allocator invariants at fixed intervals, retain raw logs and identities, and stop on the first correctness or ownership failure.

Done when:
Short fixture runs prove scheduling, interval boundaries, cancellation, injected engine failure, log identity, cleanup, and fail-fast behavior; no failed interval becomes a successful sample.

Expected touch points: soak driver, fixture engine, and focused tests.
Smallest check: the focused short-soak and injected-failure test.
Prerequisites: one-command serving, scrape snapshot, scheduler churn proof, and benchmark adapter protocol.
Owned result: reproducible soak execution and raw evidence only.
Claim: unassigned.
