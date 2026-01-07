---
title: Investigate labels lifting/argument passing bug in compiler bootstrap
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-08T04:21:35.386981+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

Review bootstrap/compiler-sbcl.lisp and bootstrap/codegen.lisp for labels lifting/argument passing issues causing labels-generated LAMBDA-9 in habu0.lisp to get only 2 list elements instead of expected 3 (CADDR). Focus on labels lifting, parameter and closure env setup.
