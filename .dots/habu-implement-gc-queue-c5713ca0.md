---
title: Implement GC queue reuse
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:18:23.283660+02:00"
---

Files: src/runtime/gc.zig
Update GC to reuse queues:
- Clear queues (clearRetainingCapacity) instead of deinit
- Keep allocated between GC runs
- Track peak usage
- Grow AFTER GC completes (never during)
Dependencies: habu-design-gc-work-3ca0d9e3
Verification: no allocations during GC trace phase
