---
title: Say that a left body may never return
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T20:46:50.566573+01:00"
---

Problem: a body the check leaves to the run records no effect that says whether it returns, so its callers are judged as if it did: with ': GEN ( -- ) s" : MADE ( n -- n ) ;" evaluate-closed ;' and GEN before, ': LATE ( -- ) 0 MADE drop s" x" 1 die ; : V ( -- n ) LATE ;' loads, and the check says E-MISMATCH at V. Acceptance: no program the load accepts is rejected by the check because a word it calls stops in a left body; a diag-position case. Depends: none. Claim: unassigned.
