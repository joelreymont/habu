---
title: Port heap GC
status: active
priority: 1
issue-type: task
created-at: "\"2026-02-03T22:36:17.412565+01:00\""
---

Context: src/runtime/heap.zig:1477; cause: Heap.collectGarbage copies root Values into gc_roots and writes back after GC; fix: build RootSet (external roots range + internal root slots) and call GC.collectRootSet; add heap gc_slots (*Value) buffer + reuse test; deps: habu-add-rootset-gc-be933d07; verification: zig build test.
