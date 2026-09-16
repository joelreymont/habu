---
title: "Contain a worker's uncaught throw in its task"
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:16:06.950379+03:00"
---

Problem: lib/task.f TASK-RUNNER catches the worker xt (TCB.USER-XT @ catch) and then dies the whole process with 'task: unhandled throw' (lib/task.f TASK-RUNNER; docs/threads.md: 'Task failure is process-fatal by design until a checked result/future model exists'). A server or any long-lived program loses every task when one worker throws. SwiftForth's thread entry catches, reports through CAUGHT and ends only that thread (docs/tasking-models.md section 1). Acceptance: an uncaught throw in a worker ends that task only: the throw code is stored in a new TCB field readable through a checked word, the task state becomes DONE, the process and the other live tasks continue, and die in a worker still exits the process with its status. Files: lib/task.f, lib/task-test.f, docs/threads.md. Verify: bin/hb --load lib/task-test.f with a new case where one of two workers throws and the other completes and is joined; bin/hb --load test/run.f green. Depends: none. Ownership: lib/task.f and its test. Claim: unassigned.
