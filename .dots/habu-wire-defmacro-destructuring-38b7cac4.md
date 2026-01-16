---
title: Wire defmacro destructuring to compilation
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:44.967369+02:00"
---

src/compiler/compile.zig: Integrate destructuring into defmacro
- Depends on: dot (destructuring codegen)
- Update compileDefmacro to use destructuring parser
- Wrap macro body with generated bindings
- Maintain backward compatibility with simple params
- Add integration tests for complex macros
- Est: 20 min
