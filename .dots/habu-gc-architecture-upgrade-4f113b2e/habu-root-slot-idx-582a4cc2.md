---
title: Root slot index
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-18T21:50:53.686050+01:00\\\"\""
closed-at: "2026-02-18T22:06:37.273887+01:00"
close-reason: Cache internal GC root slots by structural signature
blocks:
  - habu-persist-gc-state-10a4377a
---

src/runtime/heap.zig:1640. Cause: rebuilding internal root slots by full hash walks each GC. Fix: maintain persistent root-slot index with dirty epochs for package/readtable tables. Why: cut GC entry overhead independent of workload.
