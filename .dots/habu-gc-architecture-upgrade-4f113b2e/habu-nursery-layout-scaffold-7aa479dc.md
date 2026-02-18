---
title: Nursery layout scaffold
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-18T21:50:53.692005+01:00\\\"\""
closed-at: "2026-02-18T22:10:20.444407+01:00"
close-reason: Add generational heap layout scaffolding APIs
blocks:
  - habu-persist-gc-state-10a4377a
---

src/runtime/heap.zig:223. Cause: single semispace forces full live-copy each collection. Fix: carve nursery semispaces plus tenured/LOS regions with explicit alloc APIs. Why: enable minor collections and reduce copy volume.
