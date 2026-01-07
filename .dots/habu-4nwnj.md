---
title: "Milestone 2: Unify ARM64 layer with keyword API"
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-09T14:30:20.962946+02:00"
closed-at: "2025-12-09T15:06:12.970111+02:00"
close-reason: ""
---

Single ARM64 encoder using keyword-based API (:x0, :x1, etc.)

Steps:
- Create src/arm64/constants.lisp
- Create unified src/arm64/asm.lisp
- Update bootstrap and habu0 to use unified source
- Delete arm64/codegen-sbcl.lisp (377K)
