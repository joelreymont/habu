---
title: Pass records with float members by value
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T00:00:48.508697+01:00"
---

Problem: STRUCT-ARG and STRUCT-RESULT (src/lib/ffi-abi.f, the FUNCTION: header) take a byte count only, so a record with float members (an HFA, passed in v registers) cannot be declared: a C function taking or returning a struct of doubles (CGPoint, CGRect) cannot be called. Settled form: a declaration writes a by-value record as one struct<…> token listing its member types, and the word takes and returns the members as cells: `FUNCTION: MID hb_mid ( struct<r,r> struct<r,r> -- struct<r,r> ) ;FUNCTION` defines `MID ( r r r r -- r r )`; the byte-count clauses go. Acceptance: a record declared that way passes and returns by value by AAPCS64's and Apple's rules, an HFA in v registers; a test calls a C function taking and returning a struct of two doubles; Zen's sql.f declarations are converted by Zen's owner. Depends: habu-print-the-load-2dad70bd, habu-check-fn-s-6d1851d7 (check.f reads FUNCTION: through the load's reader, so the grammar is written once). Claim: unassigned.
