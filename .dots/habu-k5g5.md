---
title: Investigate labels/closure lifting bug in habu bootstrap (compiler-sbcl.lisp, codegen.lisp)
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-08T04:22:59.245793+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

Review labels lifting leading to LAMBDA-9 receiving 2-element list but CADDR accessed. Focus on closure env/arg passing.
