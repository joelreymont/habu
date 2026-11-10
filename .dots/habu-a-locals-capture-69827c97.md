---
title: A locals capture of an open layout refuses with no reason
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:24:27.464804+02:00"
---

Problem: with 'PRODUCT rvbox<a> FIELD v a ;PRODUCT' and 'PRODUCT pr2 FIELD x n FIELD y n ;PRODUCT', ': X ( rvbox<a> pr2 a -- n ) {: x y z :} 0 ;' is refused E-REJECTED, unknown_rejection, at ':}' on both engines of fix two of habu-a-stack-word-5dd345f9. Not reproduced by the lead. Found by that fix's worker (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/openlayout-fix.md): XG-READ-GROUP's open-layout arm, reached through LOC-BIND-GROUPS, sets no reason. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first, on master after fix two lands: what a locals capture of an open layout alone answers, and what this one should. Acceptance: the capture is refused with the reason and repair class the same capture gives without the expanded layout beside it. Files: src/core/checker.f; a case in test/diag-position-test.f. Verify: the reproducer under --load tools/check.f -- --json-errors and --load. Depends: habu-a-stack-word-5dd345f9. Ownership: XG-READ-GROUP's refusal under a locals capture. Claim: unassigned.
