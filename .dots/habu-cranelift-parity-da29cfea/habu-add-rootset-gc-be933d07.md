---
title: Add RootSet GC
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-03T22:31:07.505729+01:00\\\"\""
closed-at: "2026-02-03T22:33:41.440567+01:00"
close-reason: Add GC.collectRootSet
---

Context: src/runtime/gc.zig:65 + src/runtime/heap.zig:1477; cause: GC.collect roots are passed by value (requires copying + write-back); fix: add GC.collectRootSet(rootset.RootSet) that updates root slots/ranges in place; add unit tests; deps: habu-add-rootset-types-cf9db4d2; verification: zig build test.
