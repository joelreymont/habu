---
title: Design defmacro parameter destructuring
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:44.057080+02:00"
---

src/compiler/compile.zig: Plan destructuring implementation
- Parse nested parameter lists in defmacro
- Generate let bindings for destructured parameters
- Support &optional, &key, &rest in nested lists
- Handle edge cases (nil, non-list args)
- Write design doc in docs/macro-destructuring.md
- Est: 20 min
