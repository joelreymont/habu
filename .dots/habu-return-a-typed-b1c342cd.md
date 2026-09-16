---
title: Return a typed result from a joined task
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:16:24.297028+03:00"
---

Problem: a worker's outputs can only be read through TASK:HIS user cells before TASK:KILL (docs/threads.md), and a worker error is invisible to the parent. VFX tasks return an exit code and run an AtTaskExit cleanup; lib/adt/result.f already defines result<ok,err> (docs/tasking-models.md sections 3 and 5). Acceptance: TASK:JOIN ( tcb -- result ) joins the task and returns ok with the value the worker stored in a typed result cell of its TCB, or err with the throw code stored by habu-contain-a-worker-fe0c8eb8; TASK:AT-EXIT ( xt tcb -- ) registers a cleanup that runs in the worker before its thread ends, on return and on throw. Files: lib/task.f, lib/task-test.f, docs/threads.md; lib/adt/result.f is consumed, not changed. Verify: tests for a worker returning a value, a worker throwing (the err arm carries the code) and a cleanup that runs in both cases; bin/hb --load test/run.f green. Depends: habu-contain-a-worker-fe0c8eb8. Ownership: lib/task.f and its test. Claim: unassigned.
