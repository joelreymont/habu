---
title: A linearity rejection and a scoped transport rejection carry no reason
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T18:01:19.833528+02:00"
---

Problem: src/core/checker.f LIN-CHECK (:5107) sets OK false with no reason and no FAILSET on the step path (CHECKER-STEP-UNJ :5767) and on the transport path (XPORT-APPLY :17382); C2-XPORT-REJECT (:17339) names only a scoped exclusive group on copy or drop and returns with no reason for >r, 2>r, r>, 2r> and when no scoped term is found. A rejection with no reason is E-REJECTED, unknown_rejection, pinned at the last token. Found by reading. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: a program that reaches each exit and what its packet says. Acceptance: each of those rejections names its reason and its token. Depends: none. Claim: unassigned.

Reproducer (not rerun since PRODUCT was retired; declare LP:lbox with STRUCTURE): with a closed linear record LP:lbox over a DEFLINEAR, ': X ( pr2 rvbox<LP:lbox> bool -- pr2 ) [: ( rvbox<LP:lbox> bool -- rvbox<LP:lbox> bool ) dup if 1 throw then ;] catch drop drop 1 swap drop drop ;' gives E-REJECTED, unknown_rejection, 'Inspect the token...', at the drop of the stale linear bundle.

Acceptance case: with 'STRUCTURE rvbox<a> FIELD v a ;STRUCTURE' and 'STRUCTURE pr2 FIELD x n FIELD y n ;STRUCTURE', ': X ( rvbox<a> pr2 a -- n ) {: x y z :} 0 ;' is rejected E-MISMATCH (fix_type, expected 'b', actual 'rvbox<a>') at ':}', where the same declared with PRODUCT was E-REJECTED, unknown_rejection: XG-READ-GROUP's (:18546) open-layout arm, reached through LOC-BIND-GROUPS, sets no reason. Hunt first: what a locals capture of an open layout alone answers, and what this one should. Acceptance: the capture is rejected with the reason and repair class the same capture gives without the expanded layout beside it; a case in test/diag-position-test.f.
