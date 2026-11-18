---
title: "Name the program's word in the task-live rollback error"
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T14:58:31.718786+01:00"
---

Problem: a program that calls XREF:FORGET-DEFS-FROM or XREF:HIDE-DEFS-FROM while a task is live is told `error[E-TASK-COMPILE]: `ndict!` ran while a task is live` (src/habu/habu1.f BNDSET's B-LOWER-GUARD, src/habu/kernel-x64.f's twin, quoted at docs/forth-card.md ~:603), naming a primitive only engine packages can call. Acceptance: the error names the word the program called (or no internal primitive) and says how to fix it, on arm64 and x86-64 (kernel-x64.f is carl's); the card quotes the new text; src/lib/task-test.f's TASK-TEST-LOWER-LIVE and TASK-TEST-WORKER-ACTIVATES expect it. Depends: none. Claim: unassigned.
