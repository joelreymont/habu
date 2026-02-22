---
title: Compile all eligible lambdas
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-22T19:46:39.050683+01:00\\\"\""
closed-at: "2026-02-22T19:54:11.925274+01:00"
close-reason: completed
---

src/interp/repl.zig: replace single-candidate extractJitLambdaCandidate/child_chunks[0] assumption with full candidate discovery and robust chunk matching by chunk.name/arity. Add regressions in src/tests/integration.zig for multi-defun/progn JIT registration.
