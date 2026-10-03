---
title: Spell the ptr constructor once
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T11:36:34.886143+02:00"
---

Problem: the pointer constructor's spelling is tested in three places: SIG-PTR-TOK? in src/core/checker.f (r4-deftype c1c06544), STORAGE-PTR-TOK? at src/core/layout-buffer.f:404 and STG-PTR-TOK? at src/habu/verify-source.f:932. A constructor added to the parser must reach all three by hand. Acceptance: the storage scanners ask SIG-PTR-TOK? (or one shared word every reader names), the two copies are gone, every storage and checker suite and the engine two-generation build pass. Files: src/core/layout-buffer.f, src/habu/verify-source.f, src/core/checker.f.
