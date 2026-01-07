---
title: h0-eval-dispatch uses integer case without type safety
status: closed
priority: 2
issue-type: bug
assignee: ""
created-at: "2025-12-16T09:46:30.034735+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

Lines 1924-2067: 143-line case statement dispatching on integer IDs (1-209). No compile-time exhaustiveness check. Should use typed ADT with match.
