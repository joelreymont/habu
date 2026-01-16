---
title: Add macro introspection primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:33.420358+02:00"
---

src/runtime/primitives/primitives.zig: Implement macro functions
- macro-function: get macro expansion function
- macroexpand: expand macro call once
- macroexpand-1: expand macro call repeatedly
- special-operator-p: test if special form
- constantp: test if form is constant
- Add tests for expansion behavior
- Est: 25 min
