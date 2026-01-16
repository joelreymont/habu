---
title: Add CLOS initialize-instance method
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:45.269997+02:00"
---

src/compiler/compile.zig: Add initialize-instance hook
- Define generic function initialize-instance
- Call after make-instance allocates object
- Default method sets slots from initargs
- Allow user methods to customize initialization
- Add tests for initialization customization
- Est: 25 min
