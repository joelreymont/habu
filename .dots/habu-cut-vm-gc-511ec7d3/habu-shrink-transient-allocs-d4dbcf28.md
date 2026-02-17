---
title: Shrink transient allocs
status: open
priority: 1
issue-type: task
created-at: "2026-02-17T22:23:16.871089+01:00"
---

src/interp/vm.zig and src/compiler/compile.zig hot paths. Cause: repeated short-lived allocations thrash allocator/cache. Fix: stack buffers/arena reuse in hot loops.
