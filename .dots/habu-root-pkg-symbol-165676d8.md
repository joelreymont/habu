---
title: Root package symbol table values
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T10:23:19.009496+02:00"
---

src/runtime/heap.zig:775 collectGarbage - After rooting symbols/keywords:
1. Iterate packages.valueIterator(), for each package iterate symbols.iterator()
2. Append each symbol Value to roots
3. Track root index mapping for rebuild (use ArrayList to map pkg->start_idx)
Verification: Build succeeds, no segfault on GC
