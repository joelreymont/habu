---
title: Route scheduler results
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:12:14.728994+02:00"
blocks:
  - habu-apply-scheduler-result-49a5b86e
  - habu-infer-scheduler-bounded-53574658
---

Why: one server-owned result table and byte arena need an exact scheduler composition. Interface: package-private SERVE:TICK-AND-ROUTE calls SCHED:TICK once into server-owned storage, then calls CHECK-BATCH and APPLY-BATCH once. Healthy success returns the updated server. A cleanup-failed TICK still carries the complete committed row set; TICK-AND-ROUTE checks and applies that complete set before returning stop-only(server-terminal,error). A CHECK-BATCH refusal changes no connection and returns the exact server, results, and scheduler or terminal owner. Owner: scheduler tick, batch-route ordering, and transition into the server STOP-only state only. Production red: committed scheduler rows cannot reach their connections or survive cleanup refusal. Acceptance: phase instrumentation proves one TICK, one whole-set check, one whole-set apply, no apply after refusal, and complete apply before stop-only for token, terminal, failed, mixed, and cleanup-failed batches. Forbidden: row validation, row application, independent terminal type, poll, socket I/O, copied result schema, second writer, retry, thread, event queue, version, compatibility, metric, or lint. Smallest owning check: focused real-SCHED phase traces on DGX Spark. Claim: unassigned.
