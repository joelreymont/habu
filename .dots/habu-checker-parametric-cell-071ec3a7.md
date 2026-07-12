---
title: "checker: parametric cell families + TK-EVIDENCE pointees ungoverned"
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T00:09:56.920858+02:00"
---

TK-CELL capability scrutiny (2026-07-13): the NOMPTR-BLOCK? guard scopes to arity-0 TK-CELL only. Parametric cell families (span, matrix, field, ...) and TK-EVIDENCE pointees keep the old ungoverned variable->ptr-family unification. Same-class extension if/when those families need governed storage: widen NOM-SCALAR? per kind with per-caller reasoning (the arity-0 restriction exists because type ARGS in non-strict positions must keep unifying). Evaluate against the V2 plan's R6/R7 (region ownership, evidence families) before implementing - V2 may supersede with its own storage discipline.
