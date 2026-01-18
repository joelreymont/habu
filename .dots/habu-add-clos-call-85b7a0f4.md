---
title: Add CLOS call-next-method stub
status: open
priority: 2
issue-type: task
created-at: "2026-01-18T06:24:18.928766+02:00"
---

Files: src/runtime/primitives/clos.zig
Add primCallNextMethod function that:
- Takes no args
- Returns error.NotImplemented for now
Add to vm.zig opcodes or make callable.
Verify: grep for call-next-method finds implementation.
Est: 15min
