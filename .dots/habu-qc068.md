---
title: habu0.lisp builtin dispatch uses intern strings instead of typed enum
status: closed
priority: 2
issue-type: bug
assignee: ""
created-at: "2025-12-16T09:46:29.337274+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

Lines 2155-2198 create dispatch table with (intern "SYMBOL") for every entry. Brittle - should use typed enum ADT with exhaustive match.
