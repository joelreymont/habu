---
title: A mismatch names its type variables one way in JSON and another in prose
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T19:25:35.131147+02:00"
---

Problem: for ': X ( rvbox<a> -- ) dup ;' (PRODUCT rvbox<a> FIELD v a ;PRODUCT) the packet under tools/check.f --json-errors says expected 'b', actual 'rvbox<a>', and the load's prose for the same refusal says 'expected: a actual: rvbox<b>'; reported true of every data word, not rerun by the lead. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: where each renderer names the variables of the two rows (one naming pass per row, or one over both) and which reading matches the definition's own signature letters. Acceptance: the JSON and the prose of one refusal print the same expected and actual, and a variable the signature names keeps its letter. Depends: none. Claim: unassigned.

Acceptance case: for a refusal whose rows hold two generic bundles, the JSON packet's expected row prints zpr<c,d> and its actual row zpr<a,b> while the prose line prints zpr<a,b> in expected and zpr<c,d> in actual. Program g2d: `: T2M ( zpr<a,b> zpr<n,n> bool -- ) ... ;  : SN-G2D ( n zpr<c,d> -- ) 1 2 ZPR-MK 3 T2M drop ;` with /home/joel/Work/reviews/habu-10x-2026-11-10/cases8/rowclass/cover/lh-prelude.f. Acceptance: a hunt names as path:line why the two renders letter the variables differently; then prose and JSON print the same letters for the same row, and a test pins g2d in both.
