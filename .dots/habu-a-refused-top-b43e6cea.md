---
title: A refused top-level step writes no packet under --all-errors
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:15:08.252799+02:00"
---

Problem: under tools/check.f --json-errors --all-errors a top-level '1 UFB' that is refused gets no packet (/Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/pin-review/p8/s2.f and p3/all.f), on master and on habu-a-refusal-s-4e54728f's engine alike. Found by that lane's review (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/pin-rows-review.md, outside the brief). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: every refusal --all-errors meets writes its packet, a top-level step's in the shape habu-a-top-level-adb73a06 settles; a case pins it. Files: tools/check-verify-core.f, src/core/render.f, test/diag-position-test.f. Verify: the two programs. Depends: habu-a-top-level-adb73a06. Ownership: the checker's diagnostics. Claim: unassigned.
