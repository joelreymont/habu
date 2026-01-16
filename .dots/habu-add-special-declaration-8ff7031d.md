---
title: Add special declaration handling
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:43.149037+02:00"
---

src/compiler/compile.zig: Implement special variable declarations
- Depends on: dot (declare special form)
- Mark variables as special (dynamic binding)
- Use dynamic lookup instead of lexical for special vars
- Add tests for special variable semantics
- Est: 25 min
