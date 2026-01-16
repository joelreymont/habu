---
title: Implement defmacro destructuring parser
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:44.364146+02:00"
---

src/compiler/compile.zig: Parse nested parameter lists
- Depends on: dot (destructuring design)
- Add parseDestructuringParams function
- Build tree of parameter bindings
- Support all lambda-list keywords in nested positions
- Add tests for parsing various parameter forms
- Est: 30 min
