---
title: Implement Package GC visitor
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:16:47.616058+02:00"
---

Files: src/runtime/gc.zig
Add visitPackage():
- Rewrite name Value
- Iterate symbol table, rewrite all Values
- Rehash symbol table if keys moved (hash depends on pointer)
- Rewrite nicknames array
- Rewrite use_list
Handle cycles (packages referencing each other).
Dependencies: habu-design-pkg-gc-3e0cb045
Verification: packages traced, hash table rehashed correctly
