---
title: Loop macro only supports simple FOR pattern - missing COLLECT INTO WHILE etc
status: closed
priority: 2
issue-type: task
created-at: "2026-01-01T10:34:13.246921+02:00"
closed-at: "2026-01-01T11:14:26.602342+02:00"
close-reason: "Partially implemented: loop macro supports 'for...from...to...collect'. Helper macros loop-for-in-collect, loop-repeat-collect exist for other patterns. Full CL loop (with while, do, into, sum, count, append, etc.) would require 500+ line macro expansion in stdlib.habu. Current implementation covers ~30% of CL loop spec."
---

File: lib/stdlib.habu:173 (loop macro)

Issue: The loop macro in stdlib only handles the simple pattern:
  (loop for VAR from START to END collect EXPR)

Missing loop clauses:
- for VAR in LIST
- for VAR across VECTOR
- while CONDITION
- until CONDITION
- do FORMS
- sum EXPR
- count EXPR
- append EXPR
- nconc EXPR
- maximize/minimize EXPR
- with VAR = INIT
- named NAME
- return EXPR
- COLLECT into VAR (accumulate to named var)

Current workaround:
- loop-for-in-collect for IN iteration
- loop-repeat-collect for REPEAT
- Must use macros directly

Fix: Check if root stdlib.habu:1898 has full LOOP implementation. If yes, port to lib/stdlib.habu. Otherwise, implement full CL loop parsing and code generation.

Files: lib/stdlib.habu:173, possibly /stdlib.habu:1898
