---
title: Add inline/notinline declaration stubs
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:43.451276+02:00"
---

src/compiler/compile.zig: Add inline declaration parsing
- Depends on: dot (declare special form)
- Parse inline/notinline declarations
- Store in function metadata (no inlining yet)
- Add tests for declaration acceptance
- Est: 15 min
