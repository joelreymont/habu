---
title: Delete what the declaration parts no longer need
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T14:41:54.666667+01:00"
closed-at: "2026-11-16T04:07:36.284508+01:00"
---

Problem: with the declaration layer inside TYPE-DECL, twelve DECL-EVENT public words (src/core/decl-event.f:571-598: OPEN, PAYLOAD-WIDTH@, PAYLOAD-CELLS, PUBLISH, PREPARE, COMMIT, FINALIZE, ROLLBACK, RESET, CURRENT-VARIANT, NO-VARIANT, DEPTH) have no caller, and STRUCTURE-MAKE:ARM (structure-make.f:133) and GENERATED-DECL-CTOR:ARM (generated-declaration.f:467) test DECLARATION-TRANSACTION:ACTIVE? although every caller runs inside the transaction. Acceptance: the words, the two tests and ACTIVE? are gone; the declaration suites pass. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-w54decl.
