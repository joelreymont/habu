---
title: Convert all codegen to use register keywords
status: closed
priority: 1
issue-type: task
assignee: "\"\""
created-at: "\"2025-12-04T12:34:01.178581+02:00\""
closed-at: "\"2025-12-04T13:43:00.410125+02:00\""
close-reason: "\"\""
blocks:
  - habu-yi8
---

Update compiler-sbcl.lisp and codegen.lisp to use :x0, :x1, :sp etc instead of raw numbers 0, 1, 31. Required after arm64:reg rejects raw numbers.
