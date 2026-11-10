---
title: A parse-imm row survives undefine and the redefined word inherits it
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T19:36:43.606769+02:00"
---

Problem: 'TRUSTED: EV ( -- ) s" drop" evaluate ; immediate' / 's" EV" 0 parse-imm' / 'undefine EV' / ': EV ( -- ) ;' / ': F ( x -- ) EV ;': the load refuses F with E-MISMATCH; with the commit 'Defer a body that uses an unrun immediate' tools/check.f --verify-only answers W-CHECK-DEFERRED, rc 0, and on the engine before it a candidate check of 'C5 ( -- ) EV1 NOPE' after the same undefine and redefinition of a one-token parsing EV1 answers certified because the stale row skips NOPE. Cause as read by that commit's review (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/allerrors-fixA-review.md finding 2; not rerun by the lead): the rows are keyed by symbol and neither the verifier's undefine arm (src/habu/verify-source.f UNDEFINE-WORD) nor CHECKER-UNDEFINE drops the symbol's row. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: undefine and a redefinition drop the word's parse-imm rows, as parses: rows are dropped; the two programs answer as the load does in every check mode. Files: src/core/checker.f (the parse-imm table), src/habu/verify-source.f, a case in test/diag-position-test.f. Verify: the two programs, refused as the load refuses them. Depends: none. Ownership: the parse-imm table's rows at undefine. Claim: unassigned.
