---
title: "Make crew-test's record case independent of task timing"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T17:02:27.727470+02:00"
---

Problem: src/lib/crew-test.f RECORD-CASE (:89-102) starts three tasks and asserts their TASK:SELF-N differ, but src/lib/crew.f PLACE (:121-130) reaps ended tasks (REAP :111-119) before TASK:NEW, which takes the first free record: a task that ends before the next START gives its record to the next task, so two values see the same SELF-N. Observed once in the full suite on the rename candidate (2026-11-06, load 10-40): 'expected not 4476026888 got 4476026888'; passed alone. The cause above is inferred from the code, not reproduced. Acceptance: the case keeps its three tasks alive until all three have recorded (e.g. each parks until released, as STOP-CASE's PARK does) before comparing, so the assertion cannot depend on scheduling; a reproducer that delays the second START until the first task ends fails before the change and passes after. Files: src/lib/crew-test.f. Verify: bin/hb --load test/run.f -- crew. Depends: none. Ownership: src/lib/crew-test.f. Claim: unassigned.

Seen again 2026-11-06: one of four crew-test processes run at once during a full suite, on the fold candidate at rvtyrpsk ("expected not 4430118920 got 4430118920"); the same load made the installed hb fail the deadline way (dot habu-find-why-crew-8a7def85).
