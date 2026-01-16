---
title: Add function type specifiers
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:41.633697+02:00"
---

src/types/type.zig: Support (function (arg-types) result-type)
- Add .type_function with arg/result type lists
- Update arrow type to include function type info
- Support &optional, &rest, &key in arg types
- Add tests for function type matching
- Est: 30 min
