---
title: Validate imm32 patch
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-02-02T22:28:17.927341+01:00\\\"\""
closed-at: "2026-02-02T22:45:23.314200+01:00"
close-reason: Reject imm32 overflow + test
blocks:
  - habu-fix-jit-rollback-1c02296b
---

Context: src/jit/patch.zig:160-167; cause: imm64 truncated to u32 silently; fix: reject out-of-range imm64 or require imm32; add test; deps: habu-fix-jit-rollback-1c02296b; verification: zig build test
