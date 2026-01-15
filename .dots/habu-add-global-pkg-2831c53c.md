---
title: Add global package registry
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:40:41.062014+02:00"
---

src/runtime/heap.zig: Add packages HashMap to Heap struct. Initialize with default packages: COMMON-LISP, COMMON-LISP-USER, KEYWORD. Dependencies: habu-add-pkg-type-1fb86244. Verify: heap has packages map.
