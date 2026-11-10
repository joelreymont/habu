---
title: An indexed check reports no deferral
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:53:31.193568+02:00"
---

Problem: src/habu/verify-source.f :2341 and :3027 defer a stretch only with INDEX-ON, and INDEX-SUBJECT (tools/check-verify-child-lib.f :546) never calls REPORT-DEFERRALS, so those two deferrals write no packet: the subject is deferred and its reader is told nothing. Found by the deferral census (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/deferral-census.md, section 1, Not reached). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a deferral under an indexed check writes its W-CHECK-DEFERRED packet with its cause's suggestion, or the two sites are shown unreachable and removed; a test pins which. Files: src/habu/verify-source.f, tools/check-verify-child-lib.f, test/diag-position-test.f. Verify: a subject reaching :2341 and one reaching :3027 through the indexed child. Depends: habu-too-deep-control-fa36caac. Ownership: the source pre-pass. Claim: unassigned.
