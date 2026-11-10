---
title: "A TASK:NEW task's cleanup rows outlive it at a file's end"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:13:21.947957+02:00"
---

Problem: in the process-state stack the cleanup table is carried (src/lib/task.f TASK-EXIT-QT, TASK-EXIT-LINK, TASK-EXIT-N, TASK-EXIT-FREE) and the TCB pools are scratch, so a task made by TASK:NEW at a file's top level that registered AT-EXIT rows and is still unfreed at the file's end leaves those rows taken in the program with no task holding them. No wrong answer follows: the rows are only lost to later registrations (128 in the table before a chunk is mapped). The deleted TCB-POOL-SWEEP refused such a build ('TASK:NEW task at capture'). Acceptance: either the file's end refuses a file that leaves a pooled task with rows, or the rows return to TASK-EXIT-FREE there; a case of src/lib/exit-hook-test.f builds such a file and shows which. Files: src/lib/task.f, src/lib/exit-hook-test.f. Verify: suite exit-hook. Depends: the process-state feature landing. Ownership: unassigned. Claim: unassigned.
