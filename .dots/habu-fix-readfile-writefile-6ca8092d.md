---
title: Fix readFile/writeFile error masking
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T10:23:47.971145+02:00"
---

src/interp/vm.zig:1658,1669 - Remove catch blocks, propagate IO errors:
1. Let readFile/writeFile return !Value
2. Use try in callers
3. Translate to UserError at VM execute boundary if needed
Verification: File errors propagate correctly
