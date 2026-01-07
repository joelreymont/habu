---
title: Fix build to include unchanged bootstrap files (no porting)
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-09T19:16:52.396841+02:00"
closed-at: "2025-12-09T19:23:16.655372+02:00"
close-reason: ""
---

Modify build-habu0 to concatenate: macros + arm64 + habu0-core + reg-alloc + codegen (all unchanged). Remove any ported/duplicated code from habu0.lisp.
