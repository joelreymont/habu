---
title: Add subtypep primitive
status: active
priority: 2
issue-type: task
created-at: "\"2026-01-16T13:41:39.195338+02:00\""
---

src/types/check.zig: Implement subtypep predicate
- subtypep: test if type1 is subtype of type2
- Return (values boolean certain-p)
- Handle type hierarchy (fixnum < integer < rational < real < number)
- Support compound types
- Add tests for subtype relationships
- Est: 30 min
