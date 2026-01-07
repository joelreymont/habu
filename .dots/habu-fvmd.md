---
title: Investigate MAIN+620 crash (CAR on nil) in ARM64 codegen
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-07T21:13:47.090128+02:00"
closed-at: "2025-12-07T21:18:12.544238+02:00"
close-reason: ""
---

Review reg-alloc.lisp lines 1440-1610, codegen.lisp lines 3190-3250, and habu0.lisp lines 2461-2497 for root cause of MAIN+620 crash (CAR on nil after null check) in ARM64 codegen.
