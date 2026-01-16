---
title: Add array property predicates
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:19.470624+02:00"
---

src/runtime/primitives/vector.zig: Implement array type predicates
- adjustable-array-p: test if adjustable (false for now)
- array-displacement: return displacement info (nil for now)
- array-in-bounds-p: test if subscripts valid
- array-element-type: get element type
- simple-vector-p, vectorp, simple-bit-vector-p, bit-vector-p
- Add tests for all predicates
- Est: 20 min
