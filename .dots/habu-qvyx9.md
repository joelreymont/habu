---
title: "Step 1: Remove #-sbcl accumulator codegen from codegen.lisp"
status: closed
priority: 1
issue-type: task
assignee: ""
created-at: "2025-12-09T16:08:21.201306+02:00"
closed-at: "2025-12-09T19:59:43.866226+02:00"
close-reason: ""
---

Remove the 4-arg accumulator codegen function (lines 2502-3994) and helper functions. Keep only the 3-arg linear codegen.
