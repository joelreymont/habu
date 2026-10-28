---
title: Match T0 keywords as Habu resolves them
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T13:35:40.391274+02:00"
---

T0 matches control keywords with STR= (case-sensitive); the checker and source-syntax predicates accept IF, RECURSE, THEN etc. (src/core/sumtype.f:97 uses IF/EXIT/THEN; corpus shows RECURSE x9, S\" x1). Follow-up after T0 commit 2 lanes merge. Workspace: jerry-t0 stack.
