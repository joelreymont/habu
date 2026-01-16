---
title: Rebuild package symbol tables post-GC
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T10:23:30.164185+02:00"
---

src/runtime/heap.zig:775 collectGarbage - After GC relocation:
1. For each package, save old symbol_table.map, create new empty map
2. Iterate old map, get relocated symbol from roots[idx], extract new key from Symbol.name
3. Put (new_key, relocated_val) in new map, deinit old map
Depends: habu-root-pkg-symbol-165676d8
Verification: Intern symbol, GC, lookup - no segfault, same symbol returned
