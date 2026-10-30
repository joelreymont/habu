---
title: End the C2 lifecycle halt wait when the worker ends
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T20:40:26.495611+02:00"
closed-at: "2026-10-30T21:24:32.753048+02:00"
close-reason: "landed master 02e7a9f4: TEST-WAIT:FOR-CELL in src/lib/test/wait.f; four C2 test files use it; reviewed"
---

Problem: test/c2-records-lifecycle.f:39 HALT-RESULT waits 'begin READY atomic@ 0= while TASK:PAUSE repeat'; when HALT-CALL throws before PAUSE-INSIDE sets READY (seen on T0 builds before c2init: C2-MEM:WITH-RECORDS without the init layout extras) the main task spins forever (sample: 669/717 in sched_yield through task.f PAUSE). Acceptance: the wait also ends when WORKER TASK:DONE?; a worker that ended first fails the case and shows its TASK:THROW@; same fix in every same-shape C2 lifecycle wait (c2-field-loan-lifecycle.f and any the census lists). The shared shape across the census's row (c) is decided by the lead after the census. Files: test/c2-records-lifecycle.f, test/c2-field-loan-lifecycle.f. Verify: on the pre-c2init candidate the case fails at once instead of spinning; passes on the stacked candidate. Depends: census. Ownership: those files. Claim: unassigned.

2026-10-30 21:00, census B: no word waits until a cell reaches a value or a task ended, without a deadline (nearest: hand-built loops src/lib/pg-test.f:1273, :1529 with deadlines; src/lib/task-test.f:2260). Lane: one public word in src/lib/test/wait.f beside POLL/EXPIRED that waits for (cell, wanted value, task) and throws the task's TASK:THROW@ code if the task ends first (E-TASK-NO-RESULT-style code when it ended without a throw); the worker names it with a placeholder that goes into the naming interview. It replaces the 1a loops in test/c2-records-lifecycle.f:38, test/c2-init-program.f:81, test/c2-field-loan-lifecycle.f:51 and test/c2-owner-dispose.f :407 :414 :426 :447 :464 :473. Files: src/lib/test/wait.f and those four.

2026-10-30, lane: worker in .jj-ws.noindex/jerry-wait on master@origin, brief /tmp/jerry-big/t0-wait-brief.md (placeholder TEST-WAIT:FOR-CELL), baseline /tmp/jerry-big/wait-base.
