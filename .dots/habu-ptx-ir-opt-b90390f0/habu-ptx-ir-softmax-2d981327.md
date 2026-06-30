---
title: PTX IR softmax closed form
status: closed
priority: 2
issue-type: task
created-at: "2026-06-30T09:26:11.957398+02:00"
closed-at: "2026-06-30T09:29:02.178833+02:00"
close-reason: "completed locally: PTX IR has distinct input symbols, BLOCK-SUM and B- nodes, and a softmax-backward closed-form value fixture; focused PTX suite, lints, and warm full local suite green; zed/device proof untouched"
---

Local-only child of PTX IR + opt layer. Extend lib/ptx/ir.f beyond scalar ADD/MUL/NEG with distinct input symbols plus BLOCK-SUM and B- expression nodes, then add a value fixture for the softmax backward closed form dx = y*(dy - sum(dy*y)). Verify with focused ptx-stdlib static tests, lints, and local full suite. No ptxas/CUDA/zed validation.

2026-06-30 local proof: added `PTXIR-INPUT#`, `PTXIR-BSUM`, `PTXIR-BSUB`, live marking for both block-algebra nodes, and `PTXIRT-SOFTMAX-BWD` proving the value graph for `dx = y * (dy - sum(dy*y))`. Focused PTX static suite passed; typed-local-diff-lint, dot-dep-lint, stale-status-lint, host-lint, and filemap-lint passed; warm full local suite passed 24940ms internal / 27.111s wall. Zed/device validation intentionally untouched.
