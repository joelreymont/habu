---
title: Implement Readtable GC visitor
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:17:07.641140+02:00"
---

Files: src/runtime/gc.zig
Add visitReadtable():
- Iterate macro_chars, rewrite function Values
- Iterate dispatch_readtable (nested), rewrite Values
- Rehash if table keyed by moved Values
Add readtable case to copyObject().
Dependencies: habu-design-readtable-gc-1cf5c4b0
Verification: readtables traced, functions rewritten
