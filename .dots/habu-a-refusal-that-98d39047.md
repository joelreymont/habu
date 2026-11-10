---
title: "A refusal that only clears OK still prints rows from the definition's end"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T14:33:29.386335+02:00"
---

Problem: failures that clear OK and never set FAILSET (src/core/checker.f LIN-CHECK, and the data base test in CHECK-NO-BORROW :21263) pin nothing, so their packet's rows are the live rows at the definition's end; habu-a-refusal-s-4e54728f pins only failures that set FAILSET. Found by the hunt for habu-a-refusal-s-4e54728f (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/inferred-hunt.md). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a census of every place that clears OK without FAILSET, with a program for each; each prints its rows at the refused token or is shown to print none; a test pins one per kind. Files: src/core/checker.f, src/core/render.f. Verify: the census programs, full against cut. Depends: habu-a-refusal-s-4e54728f landed. Ownership: the checker's diagnostics. Claim: unassigned.

Corrected problem 2026-11-10 (review of habu-a-refusal-s-4e54728f, finding 5): failures that clear OK without failing (src/core/checker.f :4543, :4598, :5613, :5709, :5735, :5759, :5764, :11525, :18997, :19097, :20239, :21278, :22095, :22120, :22124, :22132, :22267, :22330, :22415, :22440, :23824, :23938, :23945, :23965, :24000, :24156) are pinned by the end-of-token FAIL! (:24212) after their token has run, so a token that changed the rows first prints them afterwards (LIN-CHECK through :5363: '( ltok n -- ) drop dup' prints 'ltok n -- ltok ltok' at dup); those decided at ; (:21278 through :31399) print the definition's end rows. Line numbers are that lane's.
