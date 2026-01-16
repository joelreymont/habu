---
title: Add file manipulation primitives
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:38.283294+02:00"
---

src/runtime/primitives/io.zig: Implement file operations
- rename-file: rename/move file (return 3 values: old, new, truename)
- delete-file: remove file from filesystem
- Use Zig std.fs operations
- Handle errors appropriately
- Add tests for file operations
- Est: 20 min
