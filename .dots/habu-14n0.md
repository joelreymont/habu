---
title: Remove old accumulator codegen from compiler-sbcl.lisp
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-08T11:24:01.375369+02:00"
closed-at: "2025-12-08T14:07:09.883013+02:00"
close-reason: ""
---

The old accumulator-based codegen is dead code - we always use register allocation now. Remove it to reduce confusion and maintenance burden.
