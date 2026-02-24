---
title: Declare nqueens wrappers in setup_jit
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-24T18:24:24.712485+01:00\\\"\""
closed-at: "2026-02-24T18:35:49.124146+01:00"
close-reason: Enforce JIT bench optimize declarations
---

bench/comprehensive_bench.zig:164-176 setup_jit2 defines nqueens and bench-nqueens without (declare (optimize (speed 3) (safety 0))). This violates benchmark dual-mode contract and leaves wrappers at speed=1/safety=1 in JIT runs (confirmed via HABU_TRACE_JIT). Add declares to every JIT defun in nqueens setup_jit2 and rebaseline nqueens10 vs SBCL.
