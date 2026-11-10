---
title: The plain check names another token than the load after a pinned failure
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:30:27.294885+02:00"
---

Problem: for 's" : MADE ( -- ) ;" evaluate  : X13 ( -- ) 1 IF THEN MADEX ;' the load reports E-UNDEFINED at MADEX and tools/check.f, with and without --verify-only, reports E-MISMATCH at IF (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/unseen-review/cases/s13-mismatch-then-unproduced.f; t3b-checked-clause-missing.f shows it in a does> clause). The load gives the pin to a name it cannot compile over every earlier failure (src/core/checker.f REFUSAL-STEP); the check keeps the first pinned failure since habu-an-uncheckable-token-fc4ec7cb. Before that lane the check sent such a definition to its run stage and matched the load here, and lost the pinned failure elsewhere. Found by the review of habu-an-uncheckable-token-fc4ec7cb (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/unseen-review.md, finding 3). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names as path:line why the load ranks the uncompilable name first and what each stage knows; then one rule decides the token a definition's refusal names, the same under the load, the plain check and --verify-only, and a test pins s13 in the three modes. Files: src/core/checker.f, tools/check-verify-core.f, test/diag-position-test.f. Verify: s13, t3b. Depends: habu-an-uncheckable-token-fc4ec7cb landed. Ownership: the checker's diagnostics. Claim: unassigned.

More cases 2026-11-10 (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/unseen-refold/cases): u2a ('MADE 1 IF THEN' in an unfit definition: load E-MISMATCH at IF, check E-UNCHECKABLE at MADE) and u5 ('MADE MADEX': load E-UNDEFINED at MADEX).
