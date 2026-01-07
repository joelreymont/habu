---
title: Missing TAC handlers in reg-alloc.lisp cause silent failures
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-16T09:46:28.035822+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

reg-alloc.lisp uses case without exhaustive matching. Missing handlers for :tac-null, :tac-param, :tac-loop-start, :tac-continue. Case silently returns nil, corrupting codegen. Root cause of SIGSEGV in FIND-INTERNED.
