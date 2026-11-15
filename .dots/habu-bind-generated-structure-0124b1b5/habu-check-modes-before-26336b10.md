---
title: Check modes before a built item step
status: closed
priority: 2
issue-type: task
created-at: "2026-11-14T15:48:07.347699+01:00"
closed-at: "2026-11-14T19:06:02.558430+01:00"
---

Problem: the built-items lane's new item steps (src/core/check-walk.f ITEM-KEYWORD, ITEM-TYPE, ITEM-OPERAND in carl-items b1b88095) skip the local-declaration and field-loan mode checks DO-TOK1 runs first; the field-loan one is reachable after a CALL of WITH-FIELD. Acceptance: each item step is refused in those modes as its text token is. Depends: none. Claim: unassigned.
