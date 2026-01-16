---
title: Add package VM roots
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:16:52.459454+02:00"
---

Files: src/interp/vm.zig, src/runtime/gc.zig
Add VM roots:
- all_packages list (global package registry)
- current_package (*package*)
- keyword_package
Update collectRoots() to trace these.
Dependencies: habu-implement-pkg-gc-a0990d2a
Verification: packages rooted during GC
