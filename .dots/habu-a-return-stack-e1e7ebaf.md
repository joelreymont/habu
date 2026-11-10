---
title: "A return-stack read below the definition's cells is refused at its last token"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:33:29.362744+02:00"
---

Problem: 'r>' reading below the return-stack cells the definition pushed is refused at the definition's last token, not at the r> (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/inferred/rsbelow-full.f names '3', rsbelow2-full.f names 'drop'): src/core/checker.f CHECK-NO-BORROW (:21263) runs only at the end, and the return row has no counterpart of STEP-UNDERFLOW! (:4813), which refuses a data underflow at its token. Found by the hunt for habu-a-refusal-s-4e54728f (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/inferred-hunt.md). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the refusal names the token that reads below, with the same reason, under the load, the check and --verify-only; a test pins rsbelow and rsbelow2. Files: src/core/checker.f, test/compiler/input-underflow-refusals.f. Verify: the two cases. Depends: habu-a-refusal-s-4e54728f landed (same words). Ownership: the checker's return row. Claim: unassigned.
