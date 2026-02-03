---
title: Add heap RootSet
status: active
priority: 1
issue-type: task
created-at: "\"2026-02-03T22:41:35.651348+01:00\""
---

Context: src/runtime/heap.zig:1477; cause: VM/JIT have multiple external root ranges but Heap.collectGarbage accepts only one slice; fix: add Heap.collectGarbageRootSet(roots.RootSet) and make collectGarbage([]Value) a wrapper; add unit test for multi-range external roots; deps: habu-port-heap-gc-b1aecb37; verification: zig build test.
