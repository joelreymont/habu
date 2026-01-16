---
title: "Fix vm.zig:680 pop error masking"
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T10:23:40.924000+02:00"
---

src/interp/vm.zig:680 - Replace 'self.pop() catch Value.nil' with:
Option 1: Assert invariant (stack_top > 0 before pop)
Option 2: Add popOrNil() method that checks stack_top first
Choose based on whether this can fail legitimately
Verification: Tests pass, no silent nil injection
