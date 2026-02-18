---
title: Minor GC collector
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-18T21:50:53.706636+01:00\\\"\""
closed-at: "2026-02-18T23:12:58.581872+01:00"
close-reason: implemented minor nursery collector + safe promotion
blocks:
  - habu-remembered-set-c9541b7e
---

src/runtime/gc.zig, src/runtime/heap.zig. Cause: collector traced nursery as full semispace with no generational mode split. Fix: implement minor nursery collector (roots + remembered tenured cards) with conservative pointer-free promotion into tenured region and tenured-aware forwarding/finalization handling. Why: reduce average copied bytes now without introducing pre-tenured-GC resource leaks.
