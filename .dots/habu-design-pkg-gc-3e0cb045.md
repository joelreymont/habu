---
title: Design Package GC object
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:16:39.397324+02:00"
---

Files: src/runtime/objects.zig
Design Package as GC object:
- ObjectHeader
- name: Value (symbol)
- symbols: hash table of Values (GC-tracked)
- nicknames: []Value
- use_list: []Value (packages we use)
Consider hash table implementation that supports GC.
Verification: Package struct design complete
