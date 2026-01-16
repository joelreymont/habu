---
title: Add type-of primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:39.503749+02:00"
---

src/types/check.zig: Implement type-of function
- type-of: return type specifier for object
- Return specific type (fixnum not integer)
- Return (integer low high) for fixnums
- Return (cons type1 type2) for conses
- Add tests for various objects
- Est: 20 min
