---
title: Run one server iteration
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T10:07:44.232595+02:00"
blocks:
  - habu-poll-srv-sockets-40e133cf
  - habu-route-scheduler-results-6497781f
---

Why: the two bounded iteration phases need one public ordering rule. Result: SERVE:RUN-ONCE consumes and returns one server, calls POLL-IO once and then TICK-AND-ROUTE once, and performs no work of its own. POLL-IO uses timeout zero when scheduler work is runnable, so socket waiting cannot stall inference. A STOP-only result bypasses further phases. Owner: public single-thread iteration composition only. Production red: the proven phases have no product entry point. Acceptance: three simultaneous fragmented or slow loopback clients remain isolated; scheduler work progresses with no descriptor readiness; two requests share one NEXT-MANY call; every phase failure returns the exact server or STOP-only owner. Forbidden: poll logic, scheduler logic, result routing, thread, mutex, per-client engine, blocking I/O, busy loop, worker, general event loop, metrics, reload, plugin, version, or JSON-line transport. Smallest owning check: focused multi-client loopback integration through real SCHED and GPT-2. Claim: unassigned.
