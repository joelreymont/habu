---
title: A stripped program runs with the store guard open
status: open
priority: 4
issue-type: task
created-at: "2026-11-10T17:11:58.082469+02:00"
---

Problem: in a stripped program a store into CUR-CELL, WIDN-CELL and the package cells succeeds (rc 0), where the engine refuses it with exit 83 (/Users/joel/Work/reviews/habu-10x-2026-11-10/briefs8/reports/wlcell-hunt.md, table column 'Program'; cases /Users/joel/Work/reviews/habu-10x-2026-11-10/cases8/wlcell). Not established: whether a stripped program is meant to run unguarded (no checker runs in it) or the seal is lost in the strip. Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Hunt first: where the guard is armed (SEAL-FRIEND, src/habu/main.f:260) and why a stripped program's start does not arm it, as path:line; what docs/design.md says a stripped program guarantees. Acceptance: named by the hunt. Depends: none. Claim: unassigned.
