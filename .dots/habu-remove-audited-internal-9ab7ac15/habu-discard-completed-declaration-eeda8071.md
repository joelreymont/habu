---
title: Discard completed declaration event history
status: closed
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.698781+03:00"
closed-at: "2026-10-22T12:36:34.988494+03:00"
close-reason: "Triage 2026-10-22, DONE (STALE-CLAIM): decl-event.f:518: DEV-FINALIZE resets DEV-N at depth 0, so event scratch holds nothing durable (:685-686, :710)."
---

decl-event.f retains published rows and IDENTITY although durable consequences live in registries and all published-history consumers are tests. Reuse scratch only for current declaration, release on completion; retain provisional field lookup and atomic rollback. Existing declaration success/refusal/recovery downstream tests; remove history mirrors, matching core build/gate.
