---
title: A deferral or an unfit refusal lands on another token than the load names
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:30:27.307898+02:00"
---

Problem: on master, --verify-only places W-CHECK-DEFERRED at ';' or THEN, not at the use as docs/forth.md says, when a compile-time refusal follows a covered name (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/unseen-review/cases/s11-unseen-open-if.f, s12-unseen-stray-then.f: src/core/checker.f REFUSAL-STEP's compile-refusal branch and CLOSER-REFUSE :24001 take the pin after UNSEEN is set); and for d3-unseen-then-unfit.f the load names 'drop' where the check and --verify-only name MADE (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases7/unck workspace table tab-new.txt rows 19-21). Found by the review of habu-an-uncheckable-token-fc4ec7cb (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/unseen-review.md, older defects). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: after habu-an-uncheckable-token-fc4ec7cb's fold-in lands, rerun the three; each still wrong gets its cause as path:line and a fix, with a test. Files: src/core/checker.f, test/signature-nesting.f. Verify: s11, s12, d3 in the three modes. Depends: habu-an-uncheckable-token-fc4ec7cb landed. Ownership: the checker's diagnostics. Claim: unassigned.
