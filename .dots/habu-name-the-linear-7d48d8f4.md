---
title: Name the linear value a mover takes
status: open
priority: 2
issue-type: task
created-at: "2026-11-19T17:57:52.335552+01:00"
---

Problem: a one-cell structure with a linear field (STRUCTURE qmvlin FIELD owned qown ;STRUCTURE, qown DEFLINEAR) passed to a mover whose var is MOVED (: SN ( a -- a ) dup drop ;, an unsigned : UN dup drop ;, [: dup drop ;] execute) is rejected with the generic 'UN takes a; the stack holds qmvlin ... convert or replace the value', which names neither the linear value nor a fix that works. src/core/checker.f MOVED-TAKES? refuses a type that may be linear without recording a reason, and ONE-CELL-MET records one for a MOVED var only when the value is wider than one cell, so LAYOUT-BLOCK? falls to the generic mismatch. Acceptance: each of the three prints a rejection naming the linear value and the copy or drop, as TDMV-9's text does. Depends: none. Claim: unassigned.
