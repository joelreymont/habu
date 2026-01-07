---
title: Implement native eval for Habu
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-03T19:52:37.325336+02:00"
closed-at: "2025-12-25 07:21:40"
close-reason: "Obsolete: Zig rewrite"
---

Implement a native Lisp interpreter (EVAL) that runs in compiled Habu code. Required for: interactive REPL, STEP debugger, runtime code evaluation. Must handle all special forms and function calls without relying on SBCL.
