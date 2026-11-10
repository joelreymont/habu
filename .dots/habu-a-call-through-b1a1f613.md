---
title: A call through a defer or an executed xt hides that a word reads source
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T16:39:14.001751+02:00"
---

Problem: a word that reads the source through a deferred word or an executed xt carries no CTL-PARSES, so the check does not know it reads the tokens after it. Reproducers on master and before it (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/genrow-review.md finding 1): with 'defer READER ( -- ptr u8 n )', ': PN ( -- ptr u8 n ) parse-name ;', ': SET ( -- ) ['] PN is READER ;', 'SET': (1) ': SKIPPER ( -- ) READER 2drop ;' then 'SKIPPER FOO' loads rc 0 while both check modes refuse E-UNDEFINED-TOP-LEVEL at FOO; (2) 'parses: SKIPPER 1' is refused E-PARSES-ROW; (3) a definer MK that reads its name through READER and has a true 'generates: MK ( -- ptr n )' row is refused E-GENERATES-ROW since habu-a-generates-row-fea48eb4 landed (it loaded and verified before). The refusals are sound; the programs are true. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Design first: which calls the pre-pass treats as reading the source changes with any fix (a defer's facts describe its cell, not the word set in it). Acceptance: the three programs agree between the load and the check, or the language states that a reader behind a defer must be declared and the messages say how. Files: src/core/checker.f, src/habu/verify-source.f. Verify: the three programs in test/certify-does-definer.f or test/diag-position-test.f. Depends: none. Ownership: the checker's control facts. Claim: unassigned.
