---
title: "A refused does> clause hides its parent body's refusal"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:15:08.239720+02:00"
---

Problem: when a defining word's body and its does> clause are both refused, only the clause's refusal is reported (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/pin-review/p5/clause2-full.f under tools/check.f --json-errors), on master and on habu-a-refusal-s-4e54728f's engine alike. Found by that lane's review (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/pin-rows-review.md, outside the brief). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the first failure in source order is the one reported, or both are under --all-errors; a case pins it. Files: src/core/checker.f, test/diag-position-test.f. Verify: the program. Depends: none. Ownership: the checker's diagnostics. Claim: unassigned.
