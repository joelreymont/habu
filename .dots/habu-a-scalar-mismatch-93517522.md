---
title: "A scalar mismatch's prose ends its row without the space other rows have"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:15:08.264855+02:00"
---

Problem: src/core/render.f DIAG-PROSE's scalar arm (:1729) prints 'actual: n;' where the row arms print 'actual: n ; suggestion:', so a prose line's row ends two ways. Predates habu-a-load-s-99fed2b4; found by its review (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/prose-review.md, claim 5), which shows eight assertions in test/type-decl-suite.f rest on the trailing space. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: one form for the end of a printed row in prose, and the tests that pin it. Files: src/core/render.f, test/type-decl-suite.f. Verify: a scalar and a row mismatch under --load. Depends: habu-a-load-s-99fed2b4. Ownership: the checker's diagnostics. Claim: unassigned.
