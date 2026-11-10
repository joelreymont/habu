---
title: "A refusal that only clears OK still prints rows from the definition's end"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:33:29.386335+02:00"
---

Problem: failures that clear OK and never set FAILSET (src/core/checker.f LIN-CHECK, and the data base test in CHECK-NO-BORROW :21263) pin nothing, so their packet's rows are the live rows at the definition's end; habu-a-refusal-s-4e54728f pins only failures that set FAILSET. Found by the hunt for habu-a-refusal-s-4e54728f (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/inferred-hunt.md). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a census of every place that clears OK without FAILSET, with a program for each; each prints its rows at the refused token or is shown to print none; a test pins one per kind. Files: src/core/checker.f, src/core/render.f. Verify: the census programs, full against cut. Depends: habu-a-refusal-s-4e54728f landed. Ownership: the checker's diagnostics. Claim: unassigned.
