---
title: Refuse a check run that passes its deadline by name
status: closed
priority: 2
issue-type: task
created-at: "2026-10-02T13:49:03.867889+02:00"
closed-at: "2026-10-14T19:41:03.041054+03:00"
close-reason: "Implemented in e2ccb533: check.f reports a run deadline by name and exits 70 in both output modes, with configurable --deadline-ms and killed child tree; failing-before CLI cases and affected suites passed."
---

Problem (lane 319 r4-chkout): when a checked program runs past CHK-TIMEOUT-MS (tools/check-core.f:70), E-PROC-TIMEOUT (-2502) leaves tools/check.f through the rethrow at tools/check-core.f ~:1838 as 'hb: uncaught throw code -2502', exit 67. Acceptance: a fixture that sleeps past the deadline (with a short deadline for the test) gets one line naming the label and the deadline with check.f's documented refusal exit, in default and --json-errors modes, through the real CLI, seen failing first; the process tree is gone afterwards. Files: tools/check-core.f, tools/check-test-lib.f.

Fold 339 adds: tools/check.f -- test/host-checker-row-e2e.f hits the run deadline (rc 67, E-PROC-TIMEOUT uncaught) though the file runs to rc 0 alone: the check run's deadline is also too short for a long-running subject; the fix should say what check.f does with a subject that outlives its deadline.
