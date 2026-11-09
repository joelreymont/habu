---
title: Run exit actions a task adds
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T20:30:48.366715+02:00"
---

Problem: AT-EXIT:ADD (src/lib/at-exit.f VECTOR) and the engine's exit path both reach EXIT-HOOK-CELL through data-base, which on a task is the task's own DATA region; TASK-REGION-INIT (src/lib/task.f) does not copy the cell. So an exit action added only from a task never runs when the main thread exits: a task's FS:CLEANUP-TREE+ leaves its tree behind. Measured 2026-11-09 on the fail-a-build candidate and on the landing hb (pre-existing in fs-mutate's arming): probe 'require src/lib/fs-mutate.f require src/lib/task.f $10000 TASK:TASK W : TREE$ ( -- ptr u8 n ) s" /tmp/x/tree" ; : WORK ( -- ) TREE$ FS:CLEANUP-TREE+ 0 TASK:RETURN ; : JOIN-CODE ( -- n ) W TASK:JOIN MATCH result ok OF ENDOF err OF ENDOF ;MATCH ; : MAIN ( -- ) TREE$ FS:MAKE-DIR [: WORK ;] W TASK:ACTIVATE JOIN-CODE . cr ; MAIN' exits 0 with the tree present; the same registration from MAIN removes it. Not measured: a task that dies after the main thread added an action (inferred to run none, its region's vector being empty). Acceptance: an action added from any thread runs once at the process's exit, whichever thread exits; the probe's tree is gone; a test/exit-hook-test.f case shows it. Files: src/lib/at-exit.f, src/lib/task.f, the exit path's read of EXIT-HOOK-CELL (src/habu/layout.f names it). The fix changes failure recovery across threads, so its design goes to Joel first. Depends: none. Ownership: those files. Claim: unassigned.
