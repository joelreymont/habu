---
title: "Run library task-exit hooks beside TASK:AT-EXIT"
status: open
priority: 2
issue-type: task
created-at: "2026-09-21T19:05:53.967809+03:00"
---

Problem: lib/task.f TASK-AT-EXIT holds one quotation per task (TCB.EXIT-SLOT, the slot index plus one), so lib/aio.f ENSURE-SCRUB, which registers AIO's scrub at a task's first submission, replaces any cleanup the task registered before and is replaced by any it registers after; docs/aio.md states the caveat ('A task that submits gives up its own TASK:AT-EXIT'). A library that must run at every task's end cannot share the program's one slot. Design (decided): package TASK gains an image-wide hook table, AT-END-HOOK ( [ -- ] -- ) registering a quotation once per library (a fixed small capacity, E-TASK-STATE when full), run by TASK-END for every ending task before the task's own AT-EXIT quotation; AIO registers SCRUB once (at load or at the first LOOP-START) and drops the AIO-REGISTERED user row and the per-task registration; the scrub keeps deciding by TASK:SELF-N whether the ending task owns anything. Acceptance: lib/task-test.f: a task with its own AT-EXIT that also submits runs both at its end; lib/aio-test.f case 10 unchanged; docs/threads.md lists the hook, docs/aio.md drops the caveat. Files: lib/task.f, lib/task-test.f, lib/aio.f, lib/aio-test.f, docs/threads.md, docs/aio.md. Verify: both suites, test/run.f. Depends: none. Ownership: hazel. Claim: unassigned.
