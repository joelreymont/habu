---
title: The null value fence may fence a value read through a pointer only compared with NULL-PTR
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T20:13:39.625585+02:00"
---

Problem, inferred and not reproduced: the value fence (BASE-BLOCK? in src/core/checker.f) asks the pointee variable's kind TVK-NULL, the same kind-not-value cause as the reach rule of habu-checked-src-can-88e8a977: a comparison with NULL-PTR unifies both pointees, so a value read through a real pointer that was compared with NULL-PTR would be fenced as if read through the null. Found by the known-null design (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/known-null-design.md, issue 3). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a reproducer or the run that shows there is none; if it reproduces, the fence asks the cell's null mark and the program certifies. Files: src/core/checker.f. Verify: the program as an accepted case, failing before. Depends: habu-checked-src-can-88e8a977 (the cell mark lands first). Ownership: the value fence's question. Claim: unassigned.
