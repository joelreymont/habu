---
title: Test chunk GC correctness
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:16:01.593410+02:00"
---

Files: tests/ or inline tests in gc.zig
Add tests:
- Chunk not collected while closure alive
- GC between compile() and execute()
- Closure callable after GC (bytecode intact)
- Const pool values valid after GC
Dependencies: habu-remove-manual-chunk-23ef4db9
Verification: zig build test passes, chunks survive GC correctly
