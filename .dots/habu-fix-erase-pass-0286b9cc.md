---
title: Fix erase pass error handling
status: open
priority: 2
issue-type: task
created-at: "2026-01-16T12:08:19.000254+02:00"
---

src/compiler/passes/p08_erase.zig:25 - Uses catch return instead of try, error-handling rule violation. Use try. Low severity.
