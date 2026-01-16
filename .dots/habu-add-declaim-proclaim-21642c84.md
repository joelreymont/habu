---
title: Add declaim/proclaim primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:42.236261+02:00"
---

src/compiler/compile.zig: Implement global declarations
- declaim: compile-time global declaration
- proclaim: runtime global declaration
- Store in global declaration registry
- Support optimize, inline, notinline, type, ftype, special
- Add tests for global declarations
- Est: 25 min
