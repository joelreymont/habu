---
title: Add CLOS class introspection primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:46.785559+02:00"
---

src/runtime/primitives/clos.zig: Add class accessors
- class-of: return object's class
- find-class: lookup class by name
- class-name: get class name symbol
- Update class registry for find-class
- Add tests for class introspection
- Est: 20 min
