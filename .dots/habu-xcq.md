---
title: Fix habu-read crash (labels transformation bug)
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-03T13:36:09.487037+02:00"
closed-at: "2025-12-03T18:40:32.078766+02:00"
close-reason: ""
---

habu-read crash blocked by missing defvar support. When Stage 1 tries to compile reader.lisp, the #-sbcl section includes (defun get-intern-table () *intern-table*) but the compiler doesn't know *intern-table* is a global. Options: (1) Add defvar support, (2) Remove #-sbcl accessor functions, (3) Use existing *-ir forms. Root cause: reader conditionals are processed but defvar is not implemented.
