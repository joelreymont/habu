---
title: Admit a stored type through a quotation field
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T07:34:45.044022+01:00"
---

Problem: a word `( qb<t> -- n )`, with `STRUCTURE qb<a> FIELD f [ -- a ] ;STRUCTURE`, rejects a caller's `qb<wide>` (E-MISMATCH, expected `qb<a>`, actual `qb<wide>`), though t is named only in a stored position (docs/forth.md:1262-1267). src/core/type-family.f TFC-QUOT-ROW-IMPL pushes a field's quotation row with PUSH-LOGICAL, which raises a bare variable to one cell, also when the term only answers a linearity, width or cast question (TFAM-SCH-LIN-N, MEMBER+, INIT-FIELD-WIDTH?, TFC-VAR-PAYCELLS). Acceptance: such a question changes no variable's kind; the reproducer is admitted; `( [ -- t ] -- [ -- t ] )` given `[ -- wide ]` stays rejected; a test in test/compiler/stored-type-param.f fails first. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-w233.
