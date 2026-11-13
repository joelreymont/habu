---
title: The check refuses a program whose rendered word shadows a name it holds
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T14:30:27.281938+02:00"
---

Problem: a name the checker already holds resolves first even when a rendering statement's product shadows it (the limit stated at src/core/checker.f UNSEEN-MARK$ :14812-14817), so tools/check.f refuses a program the load accepts: /home/joel/Work/reviews/habu-10x-2026-11-10/cases8/unseen-review/cases/s2b-pkg-shadow-alone.f (a global FOO ( -- n ), a package whose evaluate renders FOO ( -- bool ), then 'FOO IF THEN') is E-MISMATCH at IF under the check and rc 0 under the load, on master; with a covered name after it (s2-pkg-shadow.f) the same since the unseen lane. Found by the review of habu-an-uncheckable-token-fc4ec7cb (/home/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/unseen-review.md, finding 2). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a design says how a held name a product may shadow is checked (the review names two: lift the limit, or keep and state it); then s2b and s2 give the load's answer under the check and --verify-only, or the limit is stated where the rule is and in docs/forth.md. Files: src/core/checker.f, docs/forth.md. Verify: s2b, s2. Depends: none. Ownership: the checker's unseen names. Claim: unassigned.
