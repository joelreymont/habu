---
title: Compile-time undefined function detection
status: closed
priority: 1
issue-type: feature
assignee: ""
created-at: "2025-12-05T13:37:44.851853+02:00"
closed-at: "2025-12-25 07:21:22"
close-reason: "Obsolete: Zig rewrite"
---

During compilation, track all function calls and verify they resolve to either: (1) a primitive/builtin, (2) a defun in the current compilation unit, or (3) a known runtime function. Emit a warning or error for undefined function references.
