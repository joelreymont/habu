---
title: Investigate labels transform parameter passing in compiler-sbcl.lisp
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-08T04:40:37.457353+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

User asked to review the Transform comment around labels lifting in bootstrap/compiler-sbcl.lisp near line 2788 and explain parameter passing issue causing LAMBDA-9 to receive wrong number of args.
