---
title: Share the ENUM and STRUCTURE declaration driver
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.929097+03:00"
---

Problem: src/core/enum-decl.f (683 lines) and src/core/structure-decl.f (689 lines) are parallel copies under different prefixes. After normalizing ED-/SD-, 121 non-comment lines are identical: MISS-CLEAR, NAME-CHECK, SKIP-BODY, RESYNC, DRIVE, GUARDED, REPLAY-END, REPLAY-OUTCOME, REPLAY-WITH, RESET, RUN and others, differing only in the terminator (;enum, ;structure) and in STRUCTURE's extra QUOT-ROLLBACK. Acceptance: one shared driver, parameterized by terminator, body and rollback, serves both front ends; each file keeps only what is specific to it; ENUM and STRUCTURE diagnostics and replay behavior unchanged. Design with the Plan agent before dispatch; Fable review. Files: src/core/enum-decl.f, src/core/structure-decl.f, a shared file if the design needs one. Verify: rebuild bin/hb if both files are in the engine prefix; enum and structure suites; bin/hb --load test/run.f. Depends: none. Ownership: listed files. Claim: unassigned.
