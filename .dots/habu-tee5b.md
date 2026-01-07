---
title: reg-alloc.lisp has 80+ string-based IR dispatch without types
status: closed
priority: 2
issue-type: bug
assignee: ""
created-at: "2025-12-16T09:46:30.67687+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

ir-to-tac and codegen use string comparison for IR tags. Should use typed IR ADT with exhaustive matching. Agent audit found 82 string comparisons.
