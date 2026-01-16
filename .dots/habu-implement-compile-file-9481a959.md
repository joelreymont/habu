---
title: Implement compile-file primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:56.787448+02:00"
---

src/compiler/compile.zig: Add file compiler
- Depends on: dot (FASL serialization)
- compile-file: compile .habu file to .hfasl
- Process top-level forms
- Separate compile-time vs load-time evaluation
- Return output pathname
- Add tests for file compilation
- Est: 30 min
