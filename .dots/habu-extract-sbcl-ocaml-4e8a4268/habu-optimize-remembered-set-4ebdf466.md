---
title: Optimize remembered-set scanning
status: open
priority: 1
issue-type: task
created-at: "2026-02-20T08:55:19.467254+01:00"
blocks:
  - habu-implement-adaptive-tenuring-8d7cbd85
---

File: src/runtime/heap.zig:1, src/runtime/gc.zig:1; cause: coarse card scanning adds mutator tax; fix: dirty-card batching, precise range iteration, skip-clean fast paths; why: reduce minor-GC root overhead.
