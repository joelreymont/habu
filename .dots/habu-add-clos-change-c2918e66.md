---
title: Add CLOS change-class primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:45.873134+02:00"
---

src/compiler/compile.zig: Implement change-class
- change-class: change object's class at runtime
- Allocate new instance of target class
- Copy compatible slot values
- Call update-instance-for-different-class
- Add tests for class change behavior
- Est: 30 min
