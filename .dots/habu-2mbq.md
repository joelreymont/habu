---
title: Investigate FNTAB closures missing entries in LAMBDA-9 crash
status: closed
priority: 2
issue-type: task
assignee: ""
created-at: "2025-12-08T03:51:07.328453+02:00"
closed-at: "2025-12-25 07:21:02"
close-reason: "Obsolete: Zig rewrite supersedes Lisp bootstrap"
---

habu0 crash unpacking FNTAB: labels closures capturing source only two elements present. Need to review reg-alloc tac-lambda-ref/tac-setvar/tac-var for closure creation/storage/loading issues.
