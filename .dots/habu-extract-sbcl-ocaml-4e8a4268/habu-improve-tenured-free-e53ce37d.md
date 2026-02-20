---
title: Improve tenured free-list allocator
status: open
priority: 2
issue-type: task
created-at: "2026-02-20T08:55:19.481475+01:00"
blocks:
  - habu-add-incremental-major-c1faa29a
---

File: src/runtime/heap.zig:1; cause: fragmentation/reuse inefficiency in tenured space; fix: size-class bins, coalescing, fast-fit fallback; why: reduce RSS and major GC cost.
