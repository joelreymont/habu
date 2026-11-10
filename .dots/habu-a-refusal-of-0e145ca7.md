---
title: "A refusal of a definition's outputs names its last token, not the semicolon"
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T15:33:18.411250+02:00"
---

Problem: a refusal of a definition's declared outputs at ';' names the last token before it or the word's name as the refused token, not ';' (cases semi0, semi1, surp0, surp1 in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/rowclass, table.txt). Found by the row-class hunt (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/rowclass-hunt.md, out of scope). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names as path:line which token the packet takes for an output refusal and why; then the packet names the token the rule settles, and a case pins it. Files: src/core/checker.f, src/core/render.f, test/signature-nesting.f. Verify: the cases. Depends: none; same words as habu-a-refusal-s-4e54728f. Ownership: the checker's diagnostics. Claim: unassigned.
