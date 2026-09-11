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

Why: the two bounded iteration phases need one public ordering rule. Result: SERVE:RUN-ONCE consumes and returns one server, calls POLL-IO once and then TICK-AND-ROUTE once, and performs no work of its own. A STOP-only result from either phase bypasses every later phase. Owner: public single-thread iteration composition only. Production red: the proven phases have no product entry point. Acceptance: an instrumented production server observes exactly one POLL-IO followed by exactly one TICK-AND-ROUTE; each success and STOP-only arm returns the exact owner and never calls a later phase after failure. Forbidden: poll logic, scheduler logic, result routing, integration fixture, thread, mutex, per-client engine, blocking I/O, busy loop, worker, general event loop, metrics, reload, plugin, version, compatibility, or JSON-line transport. Smallest owning check: focused phase-order test through SERVE:RUN-ONCE. Claim: unassigned.
