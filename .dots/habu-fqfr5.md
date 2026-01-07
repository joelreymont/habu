---
title: habu0.lisp uses CLOSURE-TAG intern instead of typed ADT
status: closed
priority: 2
issue-type: bug
assignee: ""
created-at: "2025-12-16T09:46:28.70097+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

Lines 1961, 2089, 2110 use (intern "CLOSURE-TAG") for closure tagging. Should use typed ADT constructor like (make-closure body params env).
