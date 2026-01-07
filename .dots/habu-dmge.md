---
title: Investigate LAMBDA-9 labels lifting bug
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-08T04:29:13.294275+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

LAMBDA-9 labels function tries to access CADDR of a 2-element list. Need to inspect labels lifting in bootstrap/compiler-sbcl.lisp and parameter passing to lifted labels closures.
