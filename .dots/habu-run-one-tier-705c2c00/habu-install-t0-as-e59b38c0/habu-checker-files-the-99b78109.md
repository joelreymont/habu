---
title: Checker files the bound record on a K-NAME event
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T12:42:17.569645+02:00"
---

Big-picture step 1 (/tmp/jerry-big/design.md sec. 4): a0 = REC>INDEX of the record LIVE-BIND bound, a1 = seeded flag; -1 when none. Full suite (shared checker).

Joel 2026-10-29: approved, as CHECKER-TAPE:WORD carrying only the entry index (no seeded flag: DICT answers it). Kinds drop K-: CHECKER-TAPE:WORD NUMBER FLOAT STRING CHAR COUNTED-STRING PRINTED-STRING CONTROL LOCAL-NAME LOCAL-READ LOCALS-END CONSTRUCT MATCH MATCH-ARM MATCH-OF MATCH-END TICK IS OPERAND; package name stays. Needs habu-build-a-tree-d00185b1.
