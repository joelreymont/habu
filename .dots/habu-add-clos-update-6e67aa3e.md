---
title: Add CLOS update-instance-for-different-class
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:46.175699+02:00"
---

src/compiler/compile.zig: Add class change hook
- Depends on: dot (change-class primitive)
- Define generic function update-instance-for-different-class
- Called by change-class after copying slots
- Receives previous instance as argument
- Add tests for transition customization
- Est: 20 min
