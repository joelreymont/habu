---
title: Add GC finalizer tests
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T12:09:50.337147+02:00"
---

src/runtime/gc.zig:87 - No tests for stream finalizers, FD/resource leaks can regress. Add GC finalization tests. Low severity test gap.
