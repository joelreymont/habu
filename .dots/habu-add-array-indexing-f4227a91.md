---
title: Add array indexing primitives
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-16T13:41:19.162606+02:00\""
---

src/runtime/primitives/vector.zig: Implement row-major indexing
- array-row-major-index: subscripts -> linear index
- row-major-aref: get element by linear index
- Compute index from subscripts using row-major order
- Bounds check all subscripts
- Add tests for various array shapes
- Est: 20 min
