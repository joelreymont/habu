---
title: Shrink transient allocs
status: closed
priority: 1
issue-type: task
created-at: "\"2026-02-17T22:23:16.871089+01:00\""
closed-at: "2026-02-20T00:06:45.369207+01:00"
close-reason: completed via broad compile/vm allocation refactors and regression coverage; bench-check remains green
---

src/interp/vm.zig and src/compiler/compile.zig hot paths. Cause: repeated short-lived allocations thrash allocator/cache. Fix: stack buffers/arena reuse in hot loops.
