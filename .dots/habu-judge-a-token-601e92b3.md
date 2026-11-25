---
title: "Judge a token's other inputs before a left block"
status: open
priority: 3
issue-type: task
created-at: "2026-11-25T17:13:54.023467+01:00"
---

Problem: `: RUN2 ( n [ -- n ] -- n ) MADE drop execute + ; : U8 ( n -- n ) {: a:n :} true [: a ;] RUN2 ;` is E-MISMATCH on the load (bool for n at RUN2), but the check stops at the block that escaped its locals and answers W-CHECK-DEFERRED there, so a definite failure in another input of the same token hides behind the left block. Acceptance: the check answers E-MISMATCH at RUN2 as the load does; a diag-position case fails without it. Depends: habu-accept-a-quotation-9e135eb2. Claim: unassigned.
