---
title: Add allocation hot-type sampling
status: closed
priority: 2
issue-type: task
created-at: "\"2026-02-20T08:55:19.447380+01:00\""
closed-at: "2026-02-20T09:51:55.686214+01:00"
close-reason: completed
blocks:
  - habu-add-pause-budget-92f30aad
---

File: src/runtime/heap.zig:1, bench/gc.zig:1; cause: no data on allocation mix driving GC pressure; fix: sample alloc size/type and survival rates; why: informs nursery sizing/promotion heuristics.
