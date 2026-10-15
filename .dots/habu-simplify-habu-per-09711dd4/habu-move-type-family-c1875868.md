---
title: Move type-family hooks out of checker.f
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.867252+03:00"
---

Problem: 52 of the 108 defers in checker.f are hooks with a single implementer, installed only by src/core/type-family.f, which exist only because of load order. About -150 lines. Acceptance: each such call becomes direct, by moving the caller or callee across the load order; no defer is left with exactly one installer; checker and type-family suites green. Design with the Plan agent before dispatch. Files: src/core/checker.f, src/core/type-family.f. Verify: rebuild bin/hb (checker is in the engine prefix); checker suites named by test/gate-stdlib-cases.f; bin/hb --load test/run.f (shared checker behavior). Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: listed files. Claim: unassigned.
