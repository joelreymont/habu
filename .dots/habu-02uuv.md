---
title: "Fix habu.ir package mismatch - tac.lisp expects habu.ir:frame-layout but ir.lisp uses :habu"
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-16T09:46:26.712299+02:00"
closed-at: "2025-12-16T12:49:41.722489+02:00"
close-reason: ""
---

tac.lisp references habu.ir:frame-layout but shared/ir.lisp is in-package :habu, not :habu.ir. Build fails with "Unknown type specifier: HABU.IR:FRAME-LAYOUT"
