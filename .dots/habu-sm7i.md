---
title: Investigate labels lifting argument mismatch in habu0 LAMBDA-9
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-08T04:20:16.390389+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

LAMBDA-9 (labels function) receives wrong args; crashes when accessing CADDR of 2-element list. Need to review bootstrap/compiler-sbcl.lisp and bootstrap/codegen.lisp around labels compilation and closure env setup.
