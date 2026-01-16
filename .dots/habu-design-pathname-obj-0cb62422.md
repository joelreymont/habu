---
title: Design Pathname object type
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:36.761240+02:00"
---

src/runtime/objects.zig: Design Pathname structure
- Add Pathname struct: host, device, directory, name, type, version fields
- All fields are Value (string or nil or :wild/:unspecific)
- Directory is list of components
- Add tag for Pathname in Value
- Add allocPathname to Heap
- Add tests for Pathname creation
- Est: 25 min
