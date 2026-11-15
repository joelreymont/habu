---
title: Reject ptr as a NEWTYPE base
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T23:45:20.686758+01:00"
---

Problem: 'NEWTYPE dtk ptr<n,n>' loads with exit 0, though everywhere else the internal 'ptr' family is unknown in favour of '^T'. Acceptance: the base is rejected with a help naming the '^T' spelling, as 'NEWTYPE dtk ptr' is. Depends: none. Claim: unassigned.
