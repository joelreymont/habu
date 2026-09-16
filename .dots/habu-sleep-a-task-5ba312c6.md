---
title: Sleep a task for a duration without spinning
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T02:12:55.350422+03:00"
---

Problem: Habu has no sleep word; a task that must wait a fixed time yields in a TASK:PAUSE loop against mono-ns and burns a core for the duration (lib/net/tcp4-test.f PEER-TASK, Tender fixtures). Acceptance: TASK:SLEEP ( ms -- ) parks the calling task in nanosleep (declared through FUNCTION: from the process's libc, EINTR retried against the remaining time computed from mono-ns), usable from the main task and from workers; a negative duration is a named operand refusal; tests: a 50 ms sleep measures at least 45 ms and the task's CPU time over it is near zero (getrusage or /proc/self/stat through the existing process words), and lib/net/tcp4-test.f's peer task converts to it. Files: lib/task.f, lib/task-test.f, lib/net/tcp4-test.f, docs/threads.md. Verify: the tests; test/run.f green. Depends: none. Ownership: lib/task.f. Claim: unassigned.
