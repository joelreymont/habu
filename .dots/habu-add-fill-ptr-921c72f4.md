---
title: Add fill-pointer primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:20.084406+02:00"
---

src/runtime/primitives/vector.zig: Implement fill-pointer operations
- Depends on: dot (fill-pointer struct support)
- fill-pointer: get current fill-pointer value
- vector-push: add element if room (return index or nil)
- vector-pop: remove and return last element
- Check bounds against fill-pointer, not capacity
- Add tests for push/pop sequences
- Est: 20 min
