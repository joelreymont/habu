---
title: Add declare special form
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:41.935012+02:00"
---

src/compiler/compile.zig: Implement declare
- declare: provide declarations in body
- Support type, ftype, inline, notinline, ignore, ignorable, special, dynamic-extent
- Store declarations in environment during compilation
- Declarations are compile-time only (no runtime effect for now)
- Add tests for declaration parsing
- Est: 30 min
