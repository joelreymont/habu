---
title: Add type declaration checking
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:42.540245+02:00"
---

src/compiler/compile.zig: Use type declarations
- Depends on: dot (declare special form)
- Check declared types during type checking pass
- Emit type checks for declared parameter/variable types
- Warn on type mismatches
- Add tests for type declaration enforcement
- Est: 30 min
