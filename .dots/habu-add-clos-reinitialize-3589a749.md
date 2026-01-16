---
title: Add CLOS reinitialize-instance method
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:45.571051+02:00"
---

src/compiler/compile.zig: Add reinitialize-instance hook
- Define generic function reinitialize-instance
- Called to update existing instance with new initargs
- Update slot values from provided arguments
- Add tests for instance update
- Est: 20 min
