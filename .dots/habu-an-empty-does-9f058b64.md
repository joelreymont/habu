---
title: An empty does> clause refused at ; names no token and line 1
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:16:42.399178+02:00"
---

Problem: a does> clause with an empty body refused at ';' writes a packet whose token is "" at line 1, column 1, even with the definition on line 2; a colon definition names its own name token instead. Reproducer: /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/recdoes/c2/a5b.f under tools/check.f --json-errors. Found by the review of habu-a-does-clause-d74786de. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: the packet names a token of the clause on its own line (the does> or the ';'), as a colon definition's does; a case fails before. Files: src/core/checker.f, src/core/render.f. Verify: the reproducer; test/diag-position-test.f. Depends: habu-a-does-clause-9952b472 (same positions). Ownership: the checker's diagnostics. Claim: unassigned.
