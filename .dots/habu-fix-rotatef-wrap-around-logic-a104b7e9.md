---
title: Fix rotatef wrap-around logic
status: open
priority: 2
issue-type: task
created-at: "2026-01-10T11:26:05.297594+02:00"
---

lib/stdlib.habu:1022-1028 - rotatef-assignments has wrong wrap logic.
For (rotatef a b): should be a=old-b, b=old-a
Current: doesn't properly wrap last to first.
Fix: track first temp, use it for last assignment.
