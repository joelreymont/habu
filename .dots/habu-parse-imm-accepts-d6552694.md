---
title: parse-imm accepts a row for a word that is not immediate
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T19:36:43.624240+02:00"
---

Problem: ': NI ( -- ) ;' / 's" NI" 0 parse-imm' / ': FN ( x -- ) NI ;': the load and the plain check refuse FN with E-MISMATCH; with the commit 'Defer a body that uses an unrun immediate' tools/check.f --verify-only defers it (W-CHECK-DEFERRED, rc 0). The load knows NI is not immediate and makes the row anyway. Related: an immediate with a row that is not neutral ('EVN ( n -- )') is refused by the load E-UNMODELED-IMMEDIATE and only deferred under --verify-only. Seen by that commit's review (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/allerrors-fixA-review.md finding 5; not rerun by the lead). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: parse-imm refuses a name that is not an immediate word, with a reason, at its statement; the verifier's answer for the program is the load's. Files: src/core/checker.f PARSE-IMM, a case in test/diag-position-test.f. Verify: the program, refused at the parse-imm statement under --load and tools/check.f. Depends: none. Ownership: what parse-imm admits as a row. Claim: unassigned.
