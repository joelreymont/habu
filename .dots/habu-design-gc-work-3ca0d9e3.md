---
title: Design GC work queues
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:18:15.084327+02:00"
---

Files: src/runtime/gc.zig
Design pre-allocated queues:
- gray_stack: ArrayList(Value) for trace
- root_list: ArrayList(Value) for roots
Add fields to GC struct with initial capacity.
Sizing: heap_size / 64 initial, adaptive growth.
Dependencies: habu-test-readtable-gc-37ccf565 (all GC changes done)
Verification: queues allocated, capacity calculated
