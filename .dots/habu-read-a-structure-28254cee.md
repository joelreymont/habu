---
title: "Read a structure's size inside a task"
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T17:44:49.754212+01:00"
---

Problem: a BEGIN-STRUCTURE name reads its size through create/does>, a data-region load from the calling thread's register 20, so inside a task (src/lib/task.f) it reads the task's own region and faults (src/core/structures.f :63-71 records the measurement). Layouts work around it by restating each size as a constant: task-abi.f TCB-SIZE/TCB-BYTES and SEMAPHORE-SIZE/SEMAPHORE-BYTES, task.f TASK-FACILITY-SIZE, TASK-TIMESPEC-SIZE, TASK-SEM-CHUNK-SIZE. Acceptance: a program that reads a BEGIN-STRUCTURE size inside a task gets the size; each layout has one size name; a test fails first. Depends: none. Claim: unassigned.
