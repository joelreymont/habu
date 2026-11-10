---
title: "The check refuses a definer's clause where the load refuses its head"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T18:18:51.046480+02:00"
---

Problem: when a definer's head uses a word a top-level line made (so the pre-verify defers the head) and the head is wrong, the check tool reports the clause and the load reports the head: with ': MK ( -- ) create ; parses: MK 1' / 'MK MADE' before it, ': MYK ( n -- ) MADE drop does> ( -- ) ;' gives the clause's E-MISMATCH under the check where the load says "at 'drop' expected: actual: n"; an underflowing clause gives E-INPUT-UNDERFLOW. Not reproduced by the lead. Found by the hunt of habu-the-pre-verifier-20b35328 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/openif-hunt.md, Other defects). Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first, on the tree with that dot's fix: which pass reports first and why the head's refusal is not the one relayed. Acceptance: the check tool's first packet for such a file is the load's refusal, same code, token and place. Files: named by the hunt. Verify: the reproducers in test/diag-position-test.f. Depends: habu-the-pre-verifier-20b35328 (its fix changes when a head defers). Ownership: the order of a deferred definer's refusals. Claim: unassigned.

Simpler reproducers 2026-11-10, no made word needed (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/emptyclause-review.md L4), same on both engines of that lane: ': MK ( n -- ) create does> ( -- ) ;' gives only the clause (';' at 1/35) in the plain check and --all-errors, the head under the load ("at 'create' expected: actual: n"), both under --verify-only; ': MK ( n -- ) if does> ( -- ) ;' gives one packet in plain and --all-errors, two under --verify-only.
