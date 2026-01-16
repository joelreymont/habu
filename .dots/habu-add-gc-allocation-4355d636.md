---
title: Add GC allocation detector
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:18:28.429900+02:00"
---

Files: src/runtime/gc.zig
Add debug tracking:
- gc_in_progress flag (set during trace/copy)
- Override allocator in debug to assert !gc_in_progress
- Log error if allocation during GC
Only in debug builds.
Dependencies: habu-implement-gc-queue-c5713ca0
Verification: detector catches allocations during GC
