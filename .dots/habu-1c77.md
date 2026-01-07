---
title: Separate bootstrap compiler from Habu compiler source
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-06T07:52:04.027507+02:00"
closed-at: "2025-12-06T09:12:31.66537+02:00"
close-reason: ""
---

The bootstrap compiler (SBCL) and Habu compiler source are incorrectly mixed in codegen.lisp with #+sbcl/#-sbcl conditionals. This prevents proper Stage 1 building.

Fix: Extract #-sbcl code into separate pure Habu source files. Bootstrap compiler should have NO #-sbcl sections. Habu compiler source should have NO reader conditionals.
