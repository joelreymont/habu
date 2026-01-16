---
title: Add vector-push-extend primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:20.389348+02:00"
---

src/runtime/primitives/vector.zig: Implement auto-extending push
- Depends on: dot (fill-pointer primitives)
- vector-push-extend: push and grow vector if needed
- Allocate new larger vector when full
- Copy existing elements to new vector
- Update fill-pointer and return index
- Add tests for growth behavior
- Est: 25 min
