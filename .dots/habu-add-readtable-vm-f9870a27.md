---
title: Add readtable VM roots
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:17:12.700934+02:00"
---

Files: src/interp/vm.zig, src/runtime/gc.zig
Add VM roots:
- current_readtable (*readtable*)
- standard_readtable (default/initial)
Update collectRoots() to trace these.
Dependencies: habu-implement-readtable-gc-ec826e06
Verification: readtables rooted during GC
