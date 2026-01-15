---
title: Update GC for Package objects
status: open
priority: 2
issue-type: task
created-at: "2026-01-15T20:42:57.973010+02:00"
---

src/runtime/gc.zig: Add Package case to marking/copying. Follow symbol-table, use-list pointers. Dependencies: habu-design-pkg-obj-44df0b34. Verify: packages survive GC.
