---
title: "Keep a running task's cleanup rows out of a program"
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T17:58:10.326490+01:00"
---

Problem: src/lib/task.f keeps every TASK:AT-EXIT row in the image (TASK-EXIT-QT, TASK-EXIT-LINK, TASK-EXIT-N, TCB.EXIT-SLOT), so a load whose tasks register cleanups on themselves (HTTP:START, AIO) leaves them in the program in the order the tasks raced, and two builds of one source differ by a few bytes. Only a cleanup the load's own thread registers is a declaration a program must keep. Acceptance: a started task's rows and their chain head are scratch, the main thread's are kept, and test/file-objects.f's calls sequence starts and stops an HTTP server at load with both builds the same bytes. Depends: habu-keep-the-servers-e189c5ff. Claim: unassigned.
