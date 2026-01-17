---
title: Fix vector literal function call type error
status: open
priority: 2
issue-type: task
created-at: "2026-01-17T09:32:04.753937+02:00"
---

src/compiler/compile.zig: When calling defun with vector literal #(1 2 3), type checker rejects. Works fine with variables. Reproduced: (defun id (x) x) (id #(1 2 3)) fails but (let ((v #(1 2 3))) (id v)) works. Type inference from call site broken for vector literals. Blocks stdlib loading at copy-seq.
