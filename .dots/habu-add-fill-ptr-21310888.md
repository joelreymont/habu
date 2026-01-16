---
title: Add fill-pointer support to Vector
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:19.778820+02:00"
---

src/runtime/objects.zig:Vector: Add optional fill-pointer field
- Add fill_pointer: ?usize field to Vector struct
- Update allocVector to initialize fill_pointer = null
- Update makeVector to accept optional fill-pointer
- Ensure GC traces fill-pointer correctly
- Add tests creating vectors with fill-pointers
- Est: 15 min
