---
title: "Own the task control block's stack pointer for stripped images"
status: open
priority: 3
issue-type: task
created-at: "2026-09-23T08:31:04.478061+03:00"
---

Measured by the prepare lane on 06c5c9e9: a subject that activates, joins and kills a task at load (`['] STEP W TASK:ACTIVATE` … `W TASK:KILL` at top level) is refused by the stripped link with `stripped AOT persistent data holds a pointer into memory the build mapped word=<unknown> data-off=11897768` after IMAGE-LIFECYCLE:PREPARE ran: the TCB keeps its stack mapping's address past TASK:KILL (lib/task.f TASK-RELEASE-MEM), no lifecycle hook owns that cell, and the TCB is allot storage so the refusal has no name for it. TASK:PAUSE at load is not refused (test/stripped-lifecycle-prepare-subject.f). Acceptance: a killed task's TCB holds no mapped address (KILL zeroes its stack fields) or lib/task.f registers a lifecycle hook that clears every declared TCB's process-local fields at capture; a subject with a load-time activate/join/kill links stripped and its MAIN runs the task again; the refusal names the field. Files: lib/task.f, test/. Verify: the subject, test/image-lifecycle-tasks.f, test/stripped-lifecycle-prepare.f. Ownership: hazel. Claim: unassigned.
