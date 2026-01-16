---
title: Add symbol binding predicates
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:29.783732+02:00"
---

src/runtime/primitives/symbol.zig: Implement binding tests
- boundp: test if symbol has value binding
- fboundp: test if symbol has function binding
- makunbound: remove value binding
- fmakunbound: remove function binding
- Add tests for binding state changes
- Est: 15 min
