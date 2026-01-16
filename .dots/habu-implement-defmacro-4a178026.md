---
title: Implement defmacro destructuring codegen
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:44.667066+02:00"
---

src/compiler/compile.zig: Generate destructuring bindings
- Depends on: dot (destructuring parser)
- Generate nested let/car/cdr code for parameter extraction
- Handle &optional default values in nested lists
- Handle &rest in nested lists
- Add tests for generated expansion code
- Est: 30 min
