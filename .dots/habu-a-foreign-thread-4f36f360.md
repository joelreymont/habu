---
title: A foreign thread inside an exposed region cannot be called back through a caller row
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T19:09:01.226291+02:00"
---

Problem: src/lib/task.f CALLER-REGISTER (:840) gives a thread its region once, under the process key; a C-started thread that enters an exposed region through a named row has no region of its own, so it exits 106 on a call through a caller row (src/lib/ffi-callback.f:196-199). Supporting it means the thunk's owner-claiming named entry (src/habu/habu1.f BCALLBACK-THUNK :2228) sets the key's value to the exposed region and restores it on the way out. Acceptance: a thread C started, inside an exposed task, is called back through a slot bound to TASK:CALLER-CONTEXT and runs on that exposed region; the same thread later inside a second exposed region runs on the second; a case in src/lib/ffi-callback-test.f with two exposed tasks and a nested qsort. Depends: none. Consumers: none uses TASK:EXPOSE (searched zen, tender, maki), so it waits. Claim: unassigned.
