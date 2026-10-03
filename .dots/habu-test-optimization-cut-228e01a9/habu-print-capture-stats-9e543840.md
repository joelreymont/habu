---
title: Print capture stats on one line
status: closed
priority: 4
issue-type: task
created-at: "\"2026-10-01T17:17:49.786099+02:00\""
closed-at: "2026-10-01T17:49:18.983157+02:00"
close-reason: Fixed by ttnotxms 0b810ae6 (review 201 ACCEPT)
---

Problem: `.` prints a trailing newline in this engine, so GE-PRINT-CAPTURE-STATS (test/gate-common-lib.f ~:205-206) and GE-PRINT-OUTCOME (~:202) split 'stdout bytes: 0 / 32768' across lines in every failure report. Acceptance: those words print each number inline (GT-U-TYPE, lib/test/runner.f:329, or FMT:.INT), a failing row's report shows 'stdout bytes: <n> / <cap>' on one line (shown before and after on a forced failure). Files: test/gate-common-lib.f. Verify: a forced-failure probe; test/gate-diagnostics.f rc 0.
