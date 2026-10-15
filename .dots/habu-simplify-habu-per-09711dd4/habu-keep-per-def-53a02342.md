---
title: Keep per-definition checker state in one record
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.853755+03:00"
---

Problem: about 100 per-definition globals are cleared by hand: CHECK-RESET (checker.f:23248) makes 101 stores, NEW (:3292) and CHECKER-CAPTURE-PREPARE (:27577-27584) repeat parts of it. Acceptance: per-definition state lives in one record cleared by one fill; CHECK-RESET, NEW and capture-prepare use it; checker behavior unchanged. Design with the Plan agent before dispatch (which globals belong in the record; the interaction with capture and persistence). Files: src/core/checker.f. Verify: rebuild bin/hb (checker is in the engine prefix); checker suites named by test/gate-stdlib-cases.f; bin/hb --load test/run.f (shared checker behavior). Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. Ownership: checker.f per-definition state. Claim: unassigned.
