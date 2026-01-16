---
title: Add method qualifier storage
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:47.392563+02:00"
---

src/runtime/objects.zig: Extend GenericFunction for qualifiers
- Depends on: dot (method combination design)
- Add before_methods, after_methods, around_methods lists
- Update method dispatch to check qualifiers
- Sort methods by specificity within each qualifier
- Add tests for method storage
- Est: 25 min
