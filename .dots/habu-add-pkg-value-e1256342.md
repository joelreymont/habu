---
title: Add Package Value constructor
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T11:01:14.380087+02:00"
---

src/runtime/heap.zig:56: Package struct exists but no heap.allocPackage() or Value.fromPackage(). Need to box Package as Value for findPackage to return.

Files: src/runtime/heap.zig (add allocPackage method), src/runtime/value.zig (add package tag/methods)
Expected: heap.allocPackage() returns Value wrapping Package*
Actual: no way to create Package Value
Fix: Add allocPackage similar to allocHashTable with BoxedKind.package
Depends: none
Verify: package value can be created and type-checked with .isPackage()
