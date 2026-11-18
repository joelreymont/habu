---
title: "Delete the public TASK:MAIN-BASE"
status: active
priority: 2
issue-type: task
created-at: "2026-11-18T22:22:43.005891+01:00"
---

Problem: TASK:MAIN-BASE (src/lib/task.f:1799) is a public alias of TASK's private MAIN-BASE, and nothing but tests calls it: src/lib/task-test.f:1969 and src/lib/ffi-callback-test.f:168 read and write a callback row's owner cell through it, and test/ffi-callback-child.f:20 waits on the main region's CB-OWNER. Acceptance: the public word is gone and each test checks what a program observes. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-mb.
