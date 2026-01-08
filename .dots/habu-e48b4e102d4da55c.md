---
title: Add slot-value setf expansion
status: open
priority: 2
issue-type: task
created-at: "2026-01-08T07:21:37.973711+02:00"
---

File: lib/stdlib.habu:854 - Add slot-value case to setf macro. Expand (setf (slot-value obj slot) val) to (%set-slot-value obj slot val). Depends on: %set-slot-value primitive (5c4119a7).
