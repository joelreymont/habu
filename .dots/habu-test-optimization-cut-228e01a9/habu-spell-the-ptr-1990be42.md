---
title: Spell the ptr constructor once
status: closed
priority: 2
issue-type: task
created-at: "2026-10-01T11:36:34.886143+02:00"
closed-at: "2026-10-14T19:59:35.672992+03:00"
close-reason: 9af3ef74 made both storage readers consume the shared CHECKER-TYPE-SPAN-STEP, which calls SIG-PTR-TOK? for the ptr constructor; the duplicate token predicates are gone. Its storage/checker suites and two-generation build passed.
---

Problem: the pointer constructor's spelling is tested in three places: SIG-PTR-TOK? in src/core/checker.f (r4-deftype c1c06544), STORAGE-PTR-TOK? at src/core/layout-buffer.f:404 and STG-PTR-TOK? at src/habu/verify-source.f:932. A constructor added to the parser must reach all three by hand. Acceptance: the storage scanners ask SIG-PTR-TOK? (or one shared word every reader names), the two copies are gone, every storage and checker suite and the engine two-generation build pass. Files: src/core/layout-buffer.f, src/habu/verify-source.f, src/core/checker.f.
