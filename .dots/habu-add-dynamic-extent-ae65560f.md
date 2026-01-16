---
title: Add dynamic-extent declaration stubs
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:43.752619+02:00"
---

src/compiler/compile.zig: Add dynamic-extent parsing
- Depends on: dot (declare special form)
- Parse dynamic-extent declarations
- Store in metadata (no stack allocation optimization yet)
- Add tests for declaration acceptance
- Est: 15 min
