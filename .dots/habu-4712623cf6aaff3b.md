---
title: "Integrate GC with VM: trigger GC on OOM instead of returning error, add VM stack/frames to roots, add interned symbol/keyword tables to roots"
status: closed
priority: 1
issue-type: task
created-at: "2025-12-29T09:42:23.409847+02:00"
closed-at: "2025-12-29T09:46:09.011965+02:00"
close-reason: Added collectGarbage to Heap and VM with root gathering and retry
---

files: src/runtime/heap.zig:136, src/interp/vm.zig:503, src/runtime/gc.zig:216
