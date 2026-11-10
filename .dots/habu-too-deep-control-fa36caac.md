---
title: Too-deep control and a deferred stretch suggest nothing an agent can write
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T14:16:42.388378+02:00"
---

Problem: src/core/render.f UNCK-SUGGEST$ ("Rewrite with modeled words or isolate an audited primitive.") is still the suggestion for MD-CF-DEPTH (MDIAG-SUGGEST$) and TOP-DEFERRED-DIAG (:2127): it does not say what to write, the defect habu-both-recurse-refusals-7786792b fixed for recurse. Reproducer: a definition with 33 nested IFs under tools/check.f --json-errors. Found by that lane's review. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: each of the two reasons has a suggestion naming the rewrite (split the body into named words; for a deferred stretch, what makes it checkable); a case each in test/signature-nesting.f fails before. Files: src/core/render.f, test/signature-nesting.f, docs/forth-card.md. Verify: the cases. Depends: habu-both-recurse-refusals-7786792b landed. Ownership: the checker's diagnostics. Claim: agent=bob workspace=.jj-ws.noindex/habu-too-deep-control-fa36caac

Split 2026-11-10 (bob): two lanes. The control-depth suggestion is one commit in .jj-ws.noindex/habu-too-deep-control-fa36caac. A deferral has many causes (verify-source.f DEFER-AT, QUIET-TYPE-REPORT and REPORT-DEFERRED, 22 callers) and TOP-DEFERRED-DIAG is told none, so a census in .jj-ws.noindex/bob-deferral first names each cause and what lifts it; the fix brief follows its report.
