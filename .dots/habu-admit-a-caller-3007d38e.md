---
title: Admit a caller-bound callback slot in the x86-64 thunk
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T18:23:41.237253+02:00"
---

Problem: TASK:CALLER-CONTEXT (src/lib/task.f:1878) is admitted by the arm64 callback thunk only (src/habu/habu1.f:2448 BCALLBACK-THUNK, its caller-row tests at :2492-2497). The x86-64 thunk (src/habu/kernel-x64.f:4157 CB-THUNK,) reads a caller-bound row's owner (4*n + CB-ROW-CALLER) as another thread's and exits 106 ('slot held by another thread', :4272), so src/lib/pg.f's notice processor ends an x86-64 process at the first notice; src/lib/pg-test.f:2040 therefore skips NOTICE-CASES on x86-64, and src/lib/ffi-callback-test.f skips its caller-context cases there (:156, :428). Acceptance: the x86-64 thunk admits a caller-bound row as the arm64 one does (count the thread in, find its region through CB-TLS-GET and CB-TLS-KEY, count it out), and the caller-context cases of src/lib/ffi-callback-test.f and pg-test's NOTICE-CASES run on x86-64 with their x86-64 guards removed. Depends: x86-64 work is set aside until the three consumers are released. Consumers: none meets it today (none runs on x86-64). Claim: unassigned.
