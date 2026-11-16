---
title: Judge a row on a definer whose runs are unseen
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:07:38.615718+01:00"
---

Problem: a generates: or parses: row on a word whose body runs something no reader can see (a deferred word, an xt the walk did not make) stands unjudged (src/core/check-walk.f PARSES-TARGET, CHECKER-GENERATES), so a row that lies passes --verify-only with no warning: /tmp/jerry-w204-rr/p/r4.f (MKD calls a deferred no-op, 'generates: MKD ( -- n )', 'MKD NEWW', a use of NEWW) is load 70, check 70, --verify-only 0; r3.f and r6.f hide PARSES-READS-NONE the same way. Acceptance: the three readers agree on r3, r4 and r6, or --verify-only warns where it cannot judge. Depends: none. Claim: unassigned.
