---
title: Add Chunk GC visitor
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:15:51.181776+02:00"
---

Files: src/runtime/gc.zig
Add visitChunk() function:
- Iterate chunk.const_pool
- For each Value, call visitValue() to trace/rewrite
- Bytecode is opaque, no tracing needed
Add chunk case to copyObject() switch.
Dependencies: habu-update-closure-to-2d763999
Verification: chunks traced during GC, const pool rewritten
