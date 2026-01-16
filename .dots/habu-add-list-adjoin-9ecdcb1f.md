---
title: Add list adjoin primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:22.816622+02:00"
---

src/runtime/primitives/list.zig: Implement adjoin function
- adjoin: add element to list if not already present
- Support :test/:test-not/:key parameters
- Return new list with element cons'd on front if not found
- Non-destructive operation
- Add tests for presence detection
- Est: 15 min
