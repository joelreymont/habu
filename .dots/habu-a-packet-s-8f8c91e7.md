---
title: "A packet's JSON and prose name a bundle's open arguments with opposite letters"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T16:21:44.163061+02:00"
---

Problem: for a refusal whose rows hold two generic bundles, the JSON packet's expected row prints zpr<c,d> and its actual row zpr<a,b> while the prose line prints zpr<a,b> in expected and zpr<c,d> in actual. Program g2d: `: T2M ( zpr<a,b> zpr<n,n> bool -- ) ... ;  : SN-G2D ( n zpr<c,d> -- ) 1 2 ZPR-MK 3 T2M drop ;` with /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/rowclass/cover/lh-prelude.f; on master too. Found by the review of habu-a-type-mismatch-0d7ded80 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/rowcover-review.md, Outside this commit). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names as path:line why the two renders letter the variables differently; then prose and JSON print the same letters for the same row, and a test pins g2d in both. Files: src/core/render.f. Verify: g2d under --load and --json-errors. Depends: none. Ownership: the checker's diagnostics. Claim: unassigned.
