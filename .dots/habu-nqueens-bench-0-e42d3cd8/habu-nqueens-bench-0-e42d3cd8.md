---
title: "nqueens bench: 0.72x SBCL"
status: active
priority: 2
issue-type: task
created-at: "\"\\\"\\\\\\\"\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"2026-02-11T20:15:16.908688+01:00\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\\"\\\\\\\\\\\\\\\"\\\\\\\"\\\"\""
---

0.72x SBCL. Already JIT'd. Gaps: (1) cons allocation via C-ABI call (SBCL inlines), (2) tagged arithmetic overhead in safe-p loop, (3) solve has callee-saved register pressure. Depends on inline-cons dot. src/jit/hoist_backend.zig. Est: 3h
