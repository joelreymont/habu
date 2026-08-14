---
title: Load and store a multi-cell family through a pointer
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T04:23:19.765070+02:00"
---

Found by the singletons diagnosis (95b79b3a): the chain's @ and ! move exactly ONE cell; the checker's @/! over a pointer to a multi-cell family move the FAMILY'S WIDTH. lib/report.f keeps a 3-field STRUCTURE col in a TYPED-BUFFER, so COL-AT @ is a three-cell load to the checker and a one-cell load to the chain - surfacing as E-NELAB-CALL (-8550) where a call comes next (COL-HDR@, COL-AL@) and E-NELAB-ARITY (-8303) where the return does (COL+). Minimal pair differing only in field count: one-field Q1-AT @ compiles, two-field Q2-AT @ refuses at the return. This is NOT 143c0331 (permuting a bundle already on the vector) - it is GETTING one there; and not df995899 (fields are width-1, terms==cells). Owner: elaborate.f memory staging + the word model + the IR memory schema. Acceptance: the three report.f rows and both reproducers invert; engine-vs-chain differentials over 2- and 3-field structures, load and store, weighted. Files: src/compiler/native/{elaborate,hir-word}.f. Depends: none.
