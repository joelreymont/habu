---
title: Delete arena rollback
status: active
priority: 2
issue-type: task
created-at: "2026-08-05T10:36:19.530415+02:00"
---

CG-12. src/compiler/ir/arena.f:323-330 ROLLBACK restores only the count, so reusing an ordinal after rollback makes an old ID read the new object's value (Storage.v FINDING 2). Its only consumers are tests and proof scaffolding — no production caller. Delete ROLLBACK, its proof surface, and its tests rather than adding a generation epoch for a zero-consumer operation.
Claim: agent=ir-deletions workspace=.jj-ws/habu-delete-the-canonical-f119451b
