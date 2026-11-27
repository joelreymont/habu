---
title: "Keep sibling SPARQL OPTIONALs' statement linear"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-27T16:07:11.214616+01:00"
closed-at: "2026-11-27T16:30:02.522616+01:00"
---

Problem: sibling OPTIONALs that bind one variable through several columns, each holding a nested OPTIONAL, write a statement that grows with the square of the siblings: 766,068 bytes at 48 against master's 138,368. Each is joined as it is (src/lib/sparql/emit.f AS-IS?), so every later sibling's COALESCE chain repeats its seven-function term. Acceptance: at 8, 32 and 48 siblings the statement is no larger than master's; the statements of nested OPTIONALs alone and of plain siblings are unchanged; every SPARQL test answers as before. Depends: none.
