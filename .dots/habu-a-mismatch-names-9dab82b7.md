---
title: A mismatch names its type variables one way in JSON and another in prose
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T19:25:35.131147+02:00"
---

Problem: for ': X ( rvbox<a> -- ) dup ;' (PRODUCT rvbox<a> FIELD v a ;PRODUCT) the packet under tools/check.f --json-errors says expected 'b', actual 'rvbox<a>', and the load's prose for the same refusal says 'expected: a actual: rvbox<b>'. Seen by the worker of habu-a-read-of-a035fab4's second shape (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/to-r-mismatch.md, Found outside the brief), true of every data word; not rerun by the lead. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: where each renderer names the variables of the two rows (one naming pass per row, or one over both) and which reading matches the definition's own signature letters. Acceptance: the JSON and the prose of one refusal print the same expected and actual, and a variable the signature names keeps its letter. Files: src/core/render.f, as the hunt confirms. Verify: a case in test/diag-position-test.f or test/load-reject-diag-test.f comparing the two. Depends: none. Ownership: variable names in a mismatch's rows. Claim: unassigned.
