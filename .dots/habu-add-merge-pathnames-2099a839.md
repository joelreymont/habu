---
title: Add merge-pathnames primitive
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T13:41:37.676819+02:00"
---

src/runtime/primitives/io.zig: Implement merge-pathnames
- Depends on: dot (make-pathname)
- merge-pathnames: fill missing pathname components from defaults
- Handle :wild and :unspecific components
- Return new merged pathname
- Add tests for merging behavior
- Est: 25 min
