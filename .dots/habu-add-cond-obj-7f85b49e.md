---
title: Add condition object types
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:53.447799+02:00"
---

src/runtime/objects.zig: Define condition hierarchy
- Add Condition base struct
- Add Error, Warning, SimpleError, SimpleWarning subtypes
- Store condition slots: format-control, format-arguments
- Add tag for Condition in Value
- Add tests for condition creation
- Est: 25 min
