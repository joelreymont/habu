---
title: Update readtable values post-GC
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T10:23:35.247606+02:00"
---

src/runtime/heap.zig:775 collectGarbage - After package rebuild:
1. Iterate readtable, update entry.function = roots[idx++]
2. Iterate dispatch_readtable, for each sub_table iterate and update fn values
Keys are u8 (not GC-managed), only values need update
Depends: habu-root-readtable-fn-d7ddf989
Verification: Reader macros work after GC
