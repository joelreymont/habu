---
title: Add compound type specifiers
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:40.109469+02:00"
---

src/types/type.zig: Support (and/or/not type) specifiers
- Extend Type ADT with .type_and, .type_or, .type_not
- Update type checking to handle compound types
- Update subtypep to handle compounds
- Add tests for compound type relationships
- Est: 30 min
