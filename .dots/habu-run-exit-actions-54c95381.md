---
title: Run exit actions when a task ends the process
status: open
priority: 2
issue-type: task
created-at: "2026-11-09T20:30:48.366715+02:00"
---

Problem: the engine's exit path reads EXIT-HOOK-CELL through data-base, which on a task is the task's own DATA region. src/lib/at-exit.f arms the loading thread's vector at load (VECTOR-INIT :69) and a calling thread's in ADD (ARM :52); src/lib/task.f TASK-REGION-INIT (:1121) does not copy the cell. So `die` on a task that never called AT-EXIT:ADD runs none of the process's exit actions, such as a tree the main thread registered with FS:CLEANUP-TREE+ (inferred from the code, not reproduced). In a build the same cause skips hb-build's own actions: the space's ARM, run on the task, arms the task's empty vector with the space's runner, so hb-build exits with the task's code and without its "ended the process during the build" line. This source, built by tools/hb-build.f, exits 3 and prints `bye` and `task dying`:

```
require src/lib/task.f
require src/lib/at-exit.f
$10000 TASK:TASK RV-T
: BYE ( -- ) s" bye" type cr ;
: WORK ( -- ) [: BYE ;] AT-EXIT:ADD s" task dying" 3 die ;
: GO ( -- ) [: WORK ;] RV-T TASK:ACTIVATE RV-T TASK:JOIN MATCH result ok OF drop ENDOF err OF drop ENDOF ;MATCH ;
GO
: MAIN ( -- ) ;
```

Acceptance: `die` on any thread runs every registered action once; test/exit-hook-test.f cases, each failing on the base, in which the main thread registers a tree and a task dies, and in which that build exits 74 with hb-build's line. Consumers: none meets it today (Maki's remaining cleanup registration, test/child.f:40, runs on the main thread). Depends: none. Claim: unassigned.
