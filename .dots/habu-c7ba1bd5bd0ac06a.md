---
title: Add setf support for slot-value
status: open
priority: 2
issue-type: task
created-at: "2026-01-08T06:25:36.402586+02:00"
---

File: lib/stdlib.habu - Extend setf for CLOS: (setf (slot-value obj 'slot-name) val). May need %set-slot-value primitive in compile.zig or use existing set-slot-value if it exists. Check if slot-value setter already implemented. Depends on: basic setf implementation.
