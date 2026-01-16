---
title: Add progv special form
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:31.903653+02:00"
---

src/compiler/compile.zig: Implement progv special form
- progv: bind dynamic variables at runtime
- Takes list of symbols and list of values
- Establish dynamic bindings for duration of body
- Restore previous bindings on exit
- Add IR node and bytecode opcodes
- Add tests for dynamic binding
- Est: 30 min
