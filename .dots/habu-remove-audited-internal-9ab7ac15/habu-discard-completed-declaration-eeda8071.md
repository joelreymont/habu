---
title: Discard completed declaration event history
status: active
priority: 2
issue-type: task
created-at: "2026-10-16T11:17:41.698781+03:00"
---

decl-event.f retains published rows and IDENTITY although durable consequences live in registries and all published-history consumers are tests. Reuse scratch only for current declaration, release on completion; retain provisional field lookup and atomic rollback. Existing declaration success/refusal/recovery downstream tests; remove history mirrors, matching core build/gate.
