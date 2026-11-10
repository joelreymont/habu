---
title: Too-deep control and a deferred stretch suggest nothing an agent can write
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T14:16:42.388378+02:00"
---

Problem: src/core/render.f UNCK-SUGGEST$ ("Rewrite with modeled words or isolate an audited primitive.") is still the suggestion for MD-CF-DEPTH (MDIAG-SUGGEST$) and TOP-DEFERRED-DIAG (:2127): it does not say what to write, the defect habu-both-recurse-refusals-7786792b fixed for recurse. Reproducer: a definition with 33 nested IFs under tools/check.f --json-errors. Found by that lane's review. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: each of the two reasons has a suggestion naming the rewrite (split the body into named words; for a deferred stretch, what makes it checkable); a case each in test/signature-nesting.f fails before. Files: src/core/render.f, test/signature-nesting.f, docs/forth-card.md. Verify: the cases. Depends: habu-both-recurse-refusals-7786792b landed. Ownership: the checker's diagnostics. Claim: agent=bob workspace=.jj-ws.noindex/habu-deferral-cause-fa36caac

Split 2026-11-10 (bob): two lanes. The control-depth suggestion is one commit in .jj-ws.noindex/habu-too-deep-control-fa36caac. A deferral has many causes (verify-source.f DEFER-AT, QUIET-TYPE-REPORT and REPORT-DEFERRED, 22 callers) and TOP-DEFERRED-DIAG is told none, so a census in .jj-ws.noindex/bob-deferral first names each cause and what lifts it; the fix brief follows its report.

Census 2026-11-10 (bob; report /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/deferral-census.md, programs cases8/defer): the control-depth suggestion landed (c4a12f93). A deferral has seven causes, each with a rewrite the census proved: an undeclared parser, a top-level defer, a declared parser, an undeclared renderer, a rendered name, a missing type, a trusted-only tick under unknown order. Settled design: the cause is recorded where it is decided and reaches the packet's writer through a plain published checker word; src/core/checker-owner-abi.f gains nothing. Two commits: the causes the source pre-pass decides (DEFER-AT, QUIET-TYPE-REPORT), then the causes a body's check decides (REPORT-DEFERRED, verdict 2), after habu-an-uncheckable-token-fc4ec7cb lands, since both change the same checker words.

Commit one 2026-11-10 (kmqnxymz in the workspace; /Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/defer-cause-done.md): in review. TOP-DUE keeps the general text: TOP-VERDICT answers 3 for a rendered name and a missing type alike, and telling them apart needs a new reader, which commit two settles with the body sites. The census is wrong on q-newtype.f (its widget defers through TOP-DUE, not :2661).
