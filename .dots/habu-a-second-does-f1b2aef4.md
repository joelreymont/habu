---
title: A second does> in a clause is refused with no JSON packet
status: open
priority: 3
issue-type: task
created-at: "2026-11-10T19:13:08.233421+02:00"
---

Problem: ': MK ( -- ) create ; parses: MK 1' / 'MK MADE' then ': MYK ( n -- ) MADE drop create , does> ( -- ) create does> ( -- ) ;': 'tools/check.f -- --json-errors' prints only 'does> at <file>:3' and ends rc 70, no packet. Run by the review of habu-the-pre-verifier-20b35328 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/openif-review.md O4), same on master; not rerun by the lead. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: which word refuses the second does> and why the refusal bypasses the packet renderer; whether the same holds without the made word in the head. Acceptance: the refusal is one packet naming the second does> with its line, column and bytes, in every check mode, and the load names the same token. Files: named by the hunt. Verify: the reproducer in test/diag-position-test.f. Depends: none. Ownership: the refusal of a second does>. Claim: unassigned.
