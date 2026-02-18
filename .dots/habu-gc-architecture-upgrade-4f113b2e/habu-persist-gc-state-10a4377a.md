---
title: Persist GC state
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-18T21:50:33.278504+01:00\\\"\""
closed-at: "2026-02-18T21:55:51.176456+01:00"
close-reason: Persist collector state and reuse work queue
---

src/runtime/heap.zig:1700 and src/runtime/gc.zig:20. Cause: collector state/work queues recreated every GC cycle. Fix: keep collector state in heap and reuse queue capacity across collections. Why: remove allocator churn from hot GC path.
