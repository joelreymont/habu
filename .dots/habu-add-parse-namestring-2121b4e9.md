---
title: Add parse-namestring primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:38.585667+02:00"
---

src/runtime/primitives/io.zig: Implement parse-namestring
- Depends on: dot (Pathname object type)
- parse-namestring: convert string to pathname
- Parse Unix-style paths (for now)
- Split into directory/name/type components
- Return pathname object
- Add tests for path parsing
- Est: 30 min
