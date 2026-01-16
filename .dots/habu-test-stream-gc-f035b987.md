---
title: Test Stream GC correctness
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:16:33.670922+02:00"
---

Files: tests/ or inline test
Add tests:
- GC during file I/O (open, read, GC, read again)
- Stream file_path string valid after GC
- Multiple streams with interleaved GC
- Stream metadata intact after GC
Dependencies: habu-add-open-streams-271ae751
Verification: zig build test passes, streams survive GC
