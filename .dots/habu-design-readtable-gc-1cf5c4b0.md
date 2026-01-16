---
title: Design Readtable GC object
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:17:02.609227+02:00"
---

Files: src/runtime/objects.zig
Design Readtable as GC object:
- ObjectHeader
- macro_chars: array/hash of char → Value (function)
- dispatch_readtable: hash of char → hash of char → Value
Store reader macro functions as Values (closures).
Verification: Readtable struct design complete
