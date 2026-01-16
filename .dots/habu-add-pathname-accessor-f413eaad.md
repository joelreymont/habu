---
title: Add pathname accessor primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:37.372174+02:00"
---

src/runtime/primitives/io.zig: Implement pathname accessors
- Depends on: dot (Pathname object type)
- pathname-host, pathname-device, pathname-directory
- pathname-name, pathname-type, pathname-version
- Extract components from pathname object
- Add tests for component access
- Est: 15 min
