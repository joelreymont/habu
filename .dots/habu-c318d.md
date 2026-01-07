---
title: Add defconstant to habu0
status: closed
priority: 2
issue-type: feature
assignee: ""
created-at: "2025-12-08T14:51:21.058512+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

Parse like defvar, store in globals, mark as constant. Compiler can inline. Low priority since arm64/asm.lisp uses #-sbcl defun fallback.
