---
title: Root readtable function values
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T10:23:23.801999+02:00"
---

src/runtime/heap.zig:775 collectGarbage - After package symbols:
1. Iterate readtable.valueIterator(), append entry.function to roots
2. Iterate dispatch_readtable.valueIterator(), for each sub_table iterate and append function values
Verification: Readtable macros survive GC
