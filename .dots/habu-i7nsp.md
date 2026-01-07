---
title: "Fix lift-lambdas silent fallback at codegen.lisp:351"
status: closed
priority: 1
issue-type: bug
assignee: ""
created-at: "2025-12-15T16:56:31.876458+02:00"
closed-at: "2025-12-15T16:57:28.4634+02:00"
close-reason: ""
---

Line 351 has (t (cons ir lambdas)) - should error on unknown IR tag instead of passing through silently
