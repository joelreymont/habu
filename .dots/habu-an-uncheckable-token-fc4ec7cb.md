---
title: "An uncheckable token after a pinned failure overwrites the failure's reason"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:47:37.184221+02:00"
---

Problem: src/core/checker.f UNFIT-UNCHECKABLE stores UNFIT-WHY into MDIAG even when a failure is pinned, so a pinned refusal of kind RF-REASON can lose its reason. Reported by the first-failure worker; a reviewer is confirming it with a case. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a pinned failure keeps its reason whatever follows. Files: src/core/checker.f, test/signature-nesting.f. Verify: the reviewer's case. Depends: habu-unck-is-set-88095289 landed (same words). Ownership: the checker's diagnostics. Claim: unassigned.
