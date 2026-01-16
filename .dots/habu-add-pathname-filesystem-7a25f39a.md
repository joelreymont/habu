---
title: Add pathname filesystem primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:37.978944+02:00"
---

src/runtime/primitives/io.zig: Implement filesystem operations
- Depends on: dot (Pathname object type)
- truename: resolve pathname to canonical form
- probe-file: test if file exists, return truename or nil
- directory: list directory contents (return list of pathnames)
- ensure-directories-exist: create parent directories
- Add tests for filesystem interaction
- Est: 30 min
