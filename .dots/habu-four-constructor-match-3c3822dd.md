---
title: Four constructor-match refusals suggest nothing an agent can write
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:15:08.191409+02:00"
---

Problem: src/core/render.f MDIAG-SUGGEST$ has no arm for MD-CON-TRUNC, so it prints the MATCH-form suggestion (program /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/prose-review/p/contrunc.f), and MD-CON-FAM, MD-FAM-KIND and MD-CON-KIND (:1199-1201) repeat their reason instead of naming a fix; the prose line now prints each twice over. Found by the review of habu-a-load-s-99fed2b4 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/prose-review.md, separate issue b). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: each of the four has a suggestion of its own that says what to write, and its prose reason does not repeat it; a case each fails before. Files: src/core/render.f, test/signature-nesting.f. Verify: contrunc.f and a program per reason under --json-errors and --load. Depends: none. Ownership: the checker's diagnostics. Claim: unassigned.
