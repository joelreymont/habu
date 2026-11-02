---
title: Match T0 keywords as Habu resolves them
status: closed
priority: 2
issue-type: task
created-at: "2026-10-28T13:35:40.391274+02:00"
closed-at: "2026-11-02T11:34:09.160507+02:00"
close-reason: "Superseded: T0 takes names from the record the checker bound, not by spelling (src/habu/t0.f:6-9); no STR= keyword match remains."
---

T0 matches control keywords with STR= (case-sensitive); the checker and source-syntax predicates accept IF, RECURSE, THEN etc. (src/core/sumtype.f:97 uses IF/EXIT/THEN; corpus shows RECURSE x9, S\" x1). Follow-up after T0 commit 2 lanes merge. Workspace: jerry-t0 stack.
