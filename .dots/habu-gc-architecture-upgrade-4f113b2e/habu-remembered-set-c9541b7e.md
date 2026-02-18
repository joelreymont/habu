---
title: Remembered set
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-18T21:50:53.702001+01:00\\\"\""
closed-at: "2026-02-18T22:22:24.059054+01:00"
close-reason: Add remembered-set card scan and clear APIs
blocks:
  - habu-write-barrier-stores-2b8bf449
---

src/runtime/gc.zig and src/runtime/heap.zig. Cause: barrier marks currently absent so young roots would be missed. Fix: card table + remembered set scanning APIs. Why: bounded minor-GC root set and correctness.
