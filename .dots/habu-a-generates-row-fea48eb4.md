---
title: "A generates: row is accepted on a word that reads no name"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T15:22:55.102749+02:00"
---

Problem: 'generates: D ( effect )' declares a definer called as 'D NAME', but the row is accepted, with no E-GENERATES-ROW, on a word that reads no name (a renderer taking no name, evaluate, a defer). The source pre-pass then takes the next token as the product's name: e2-trust-gen.f (a trust after such a row) fails the check with E-STATEMENT-THROW 7158, rc 70, while the load accepts it, rc 0; d1-gen-noname.f and d2-eval-gen.f read ':' as the product and defer the next definition while the load refuses its undefined word. Both engines, before and after the deferral-cause commit. Found by its review (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/defer-cause-review.md, findings 1 and 6a; programs in /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/defer-review/p). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a hunt names as path:line where the row is accepted and what the pre-pass reads after the definer; then the check and the load agree on the three programs, by refusing the row or by reading no name. Files: src/core/checker.f (CHECKER-GENERATES), src/habu/verify-source.f. Verify: the three programs under --verify-only and --load. Depends: none. Ownership: the source pre-pass. Claim: unassigned.
