---
title: Refuse a checked tick of a trust-boundary word
status: open
priority: 2
issue-type: task
created-at: "2026-10-04T06:16:43.204529+03:00"
---

Problem: a checked tick of an ABI-only word (no primitive, no owner row) is admitted while a call to it is refused E-CAP-TRUSTED. checker.f OWNED-TICK-REFUSED? (:17920) refuses only when PE-OWNER-ROW (:14379) finds an owner row, and BTICK-TOK (:17933) then pushes the row's quotation type. Measured on master 9d126e86 bin/hb with /private/tmp/claude-501/-Users-joel-Work-habu/45544816-68f7-4358-b8ff-9239acfd1105/scratchpad/rev-b8/probes/p-tick.f (`1 set-tier`, PRAW defined under `0 set-check`, hook restored, `: PTICK ( n -- n ) ['] PRAW execute ;`): certifies, rc 0. The same file calling `PRAW` directly: E-CAP-TRUSTED 'PRAW' is a trust-boundary primitive, rc 70. Found by the B8 review; pre-existing on its parent product. Acceptance: a checked `[']` or `'` of a word whose call is E-CAP-TRUSTED is refused with the same code, naming the word; ticks of owned or checked words unchanged; rejected-program fixture for both tick forms beside the call case. Files: src/core/checker.f (OWNED-TICK-REFUSED?, BTICK-TOK), the E-CAP-TRUSTED fixture suite. Verify: the probe exits 70 naming PRAW; checker suites; test/run.f. Depends: none. Ownership: src/core/checker.f tick path. Claim: unassigned.
