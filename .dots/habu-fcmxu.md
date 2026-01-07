---
title: Port linear codegen + register allocator to habu0.lisp
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-09T15:54:57.424904+02:00"
closed-at: "2025-12-09T18:16:34.574649+02:00"
close-reason: ""
---

Port the complete linear codegen pipeline from bootstrap/ to habu0.lisp. This includes linearize, codegen-linear-instr, and the 5-pass register allocator. Goal is ONE set of tested code shared between SBCL bootstrap and native habu.
