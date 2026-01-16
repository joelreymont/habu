---
title: Add satisfies type specifier
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:40.416167+02:00"
---

src/types/type.zig: Support (satisfies predicate) type
- Add .type_satisfies variant with predicate function
- Test type by calling predicate at runtime
- Update typep to handle satisfies
- Add tests for custom predicates
- Est: 25 min
