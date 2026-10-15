---
title: Grow checker tables through the arena
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.892459+03:00"
blocks:
  - habu-allocate-checker-tables-24fc3077
---

Problem: checker.f defines 28 bespoke *-GROW words alongside the shared ARENA-ROWS-ENSURE and REG-GROW1 (checker.f:242, 230). About -60 lines. Acceptance: every table grows through the shared words; the bespoke growers are deleted; checker suites green. Files: src/core/checker.f. Verify: rebuild bin/hb (checker is in the engine prefix); checker suites named by test/gate-stdlib-cases.f; bin/hb --load test/run.f (shared checker behavior). Depends: the first-use allocation dot (same tables); carl-final-core chain landed. Ownership: checker.f growth words. Claim: unassigned.
