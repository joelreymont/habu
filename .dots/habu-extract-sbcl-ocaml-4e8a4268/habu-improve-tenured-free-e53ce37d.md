---
title: Improve tenured free-list allocator
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-20T08:55:19.481475+01:00\""
closed-at: "2026-02-20T14:57:08.400804+01:00"
close-reason: Completed tenured allocator bins, split policy, and fragmentation gates
blocks:
  - habu-add-incremental-major-c1faa29a
---

File: src/runtime/heap.zig:1; cause: fragmentation/reuse inefficiency in tenured space; fix: size-class bins, coalescing, fast-fit fallback; why: reduce RSS and major GC cost.
