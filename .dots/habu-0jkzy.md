---
title: Refactor h0-eval dispatch to check symbolp once, use efficient case/dispatch
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-08T19:08:39.557613+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

Current h0-eval has ~50+ cond clauses each doing `(if (symbolp op) (op=xxx op) nil)`. This is wasteful - checks symbolp repeatedly even after matching. Better approaches:
1. Check (symbolp op) once at top level, then nested cond with just (op=xxx op)
2. Use case/ecase if habu0 case works with symbols via eq
3. Build dispatch table (alist symbol->handler) for O(1) lookup
Current code wastes cycles on redundant symbolp checks after first match.
