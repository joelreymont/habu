---
title: Implement error function
status: open
priority: 2
issue-type: task
created-at: "2026-01-08T09:25:44.302842+02:00"
---

Need to implement (error msg) function for signaling runtime errors. Used by many stdlib functions for validation. Should probably be a primitive that raises a Zig error with the message, or it could be implemented in Lisp using the condition system.
