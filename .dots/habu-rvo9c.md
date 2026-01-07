---
title: "Unify IR packages: habu.ir vs habu for typed pipeline"
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-16T09:55:34.641066+02:00"
closed-at: "2025-12-25 07:21:12"
close-reason: "Obsolete: Zig rewrite"
---

The typed pipeline (bootstrap/ir.lisp) defines IR in habu.ir package. habu0.lisp uses IR in habu package (shared/ir.lisp). When building habu0 with typed pipeline, IR constructors like habu::ir-lit are unresolved because bootstrap/ir.lisp defines habu.ir::* not habu::*. Need to unify or bridge these packages.
