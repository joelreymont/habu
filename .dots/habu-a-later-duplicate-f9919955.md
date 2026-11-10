---
title: "A later duplicate is reported ahead of the run's refusal of an earlier deferred definition"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T19:13:08.219092+02:00"
---

Problem: ': MK ( -- ) create ; parses: MK 1' / 'MK MADE' then ': P ( n -- ) MADE drop if ;' twice: 'tools/check.f -- --json-errors' answers E-DUPLICATE-DEFINITION P at 4/3, rc 78, where the load refuses the first P at ';' on line 3. The same with two identical faulty definers, and with a faulty definer followed by ': MYK ( -- ) ;'. Run by the review of habu-the-pre-verifier-20b35328 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/openif-review.md O2), same on master; not rerun by the lead. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: which pass raises the duplicate, and why a definition the pre-verify deferred to the run is already recorded when the second one is read. Acceptance: the check tool's first packet for such a file is the load's refusal, same code, token and place; the duplicate is reported only where the load reports it. Files: named by the hunt. Verify: the reproducers in test/diag-position-test.f. Depends: habu-the-check-refuses-326a0490 (the same order of a deferred definition's refusals; hunt them together). Ownership: a duplicate of a deferred definition. Claim: unassigned.
