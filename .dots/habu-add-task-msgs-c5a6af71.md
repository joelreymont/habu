---
title: Add task messages and a blocking queue
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:16:24.304414+03:00"
---

Problem: tasks have no channel: no mailbox and no queue (docs/threads.md). VFX provides a one-cell mailbox per task (SEND-MESSAGE blocks while the target holds an unread message, GET-MESSAGE blocks until one arrives, MSG? tests) and a power-of-two ring queue with blocking put and get in Lib/CQueues.fth (docs/tasking-models.md section 3). Acceptance: TASK:SEND-MESSAGE ( x tcb -- ), TASK:GET-MESSAGE ( -- x tcb ) and TASK:MSG? ( tcb -- bool ) with VFX semantics but blocking on the semaphore from habu-add-a-blocking-fd79b713 instead of a PAUSE loop; and a typed bounded queue package for cell-sized elements (QUEUE:QUEUE with a size, PUSH blocking when full, POP blocking when empty, TRY-PUSH and TRY-POP non-blocking, COUNT), safe for many producers and many consumers. Files: lib/task.f, new lib/queue.f, lib/task-test.f, lib/queue-test.f, docs/threads.md. Verify: a mailbox round trip between two tasks; a queue soak with four producers and two consumers delivering every element exactly once; bin/hb --load test/run.f green. Depends: habu-add-a-blocking-fd79b713. Ownership: lib/task.f, lib/queue.f and their tests. Claim: unassigned.
