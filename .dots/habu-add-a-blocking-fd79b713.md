---
title: Add a blocking semaphore to the task package
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:16:06.957221+03:00"
---

Problem: lib/task.f offers only FACILITY (an owner-tracked mutex) and PAUSE, so a task waiting for work must poll (docs/threads.md). SwiftForth added a SEMAPHORE over sem_open/sem_wait/sem_post in 2025; VFX uses counted semaphores (REQUEST/SIGNAL) and a per-task halt semaphore so HALT/RESTART block without polling (docs/tasking-models.md sections 1 and 3). Acceptance: TASK:SEMAPHORE defines a typed counted semaphore with TASK:WAIT ( sem -- ) blocking until the count is positive then decrementing, TASK:SIGNAL ( sem -- ) incrementing and waking one waiter, and explicit init and destroy words, implemented over unnamed POSIX semaphores (sem_init/sem_wait/sem_post/sem_destroy) through the existing FFI slots; waiting on a destroyed semaphore fails with a named code. Files: lib/task.f, lib/task-test.f, docs/threads.md. Verify: a producer/consumer test where the consumer blocks without a PAUSE loop and receives N items in order; bin/hb --load test/run.f green. Depends: none. Ownership: lib/task.f and its test. Claim: unassigned.
