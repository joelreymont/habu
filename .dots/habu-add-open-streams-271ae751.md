---
title: Add open streams VM root
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T16:16:27.468036+02:00"
---

Files: src/interp/vm.zig, src/runtime/gc.zig
Add VM.open_streams list to track all open streams.
Update collectRoots() to iterate open_streams.
Update stream creation/close to add/remove from list.
Dependencies: habu-add-stream-gc-279312b9
Verification: open streams rooted during GC
