---
title: "Name the signature's variable in a layout mismatch"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T07:34:45.044022+01:00"
---

Problem: a caller that passes qb<wide> to a word declared ( qb<t> -- n ), with STRUCTURE qb<a> FIELD f [ -- a ] ;STRUCTURE, is rejected with 'expected: qb<a> actual: qb<wide>': the record spells the structure's own parameter a, not the signature's t, and does not say which use fixed t's width. Acceptance: the record names the declared signature's variable and the use that constrained it, as a program-diagnostics test. Depends: none. Claim: unassigned.
