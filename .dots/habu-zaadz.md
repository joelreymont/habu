---
title: "DRY violation: 4 locations need updating per new operator in habu0"
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-15T16:56:32.768457+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

Adding 1 operator requires 4 edits: 1) *op-* var, 2) init-*, 3) h0-compile dispatch, 4) h0-eval dispatch. Should use single dispatch table
