---
title: Route scheduler results
status: open
priority: 1
issue-type: task
created-at: "2026-07-30T01:12:14.728994+02:00"
blocks:
  - habu-own-completion-srv-dfc8812e
  - habu-apply-completion-result-021d8f78
  - habu-infer-scheduler-bounded-53574658
---

Why: one server-owned result table and byte arena need an exact tick-and-route transaction. Interface: package-private SERVE:TICK-AND-ROUTE calls SCHED:TICK once into server-owned storage, validates the complete returned row set, every byte slice, and every unique live connection through SERVE-CONN:CHECK-APPLY before mutation, then performs only total SERVE-CONN:APPLY transitions. Normal success returns the healthy server after all rows apply. A cleanup-failed terminal result still carries the complete already-committed row set: TICK-AND-ROUTE applies every row first, then returns stop-only(server-terminal,error), where server-terminal retains SCHED:terminal plus every connection, listener, buffer, and result owner. No prefix can apply. SERVE:STOP accepts that exact state and begins its connection walk with CLOSE-AFTER-SCHED-FAIL. Owner: scheduler tick, complete result routing, and transition into the existing STOP-only server state only. Production red: scheduler results cannot reach their connections and a tick-time cleanup refusal would otherwise discard committed tokens. Acceptance: token, token-final, finished, failed, stale, duplicate, unknown-handle, short-arena, and cleanup-failed cases preserve the stated owners; any preflight refusal changes no connection; cleanup failure routes the exact complete set before STOP-only and never zero or a prefix; zero, one, and several open connections in stop-only state all reach STOP and consume every descriptor; two admitted requests share one NEXT-MANY call and route to distinct connections. Forbidden: independent terminal type, poll, accept, socket I/O, copied result schema, second writer, retry, thread, event queue, version, or compatibility path. Smallest owning check: focused real-SCHED routing traces on DGX Spark.
