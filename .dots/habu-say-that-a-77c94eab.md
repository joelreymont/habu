---
title: Say that a left body may never return
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T20:46:50.566573+01:00"
---

Problem: a body the check leaves to the run records nothing that says whether it returns, so its callers are judged as if it did, both ways. With ': GEN ( -- ) s" : MADE ( n -- n ) ;" evaluate-closed ;' and GEN before, ': LATE ( -- ) 0 MADE drop s" x" 1 die ; : V ( -- n ) LATE ;' loads, and the check says E-MISMATCH at V. With ': GEN ( -- ) s" : MADE ( n -- n ) dup drop ;" evaluate-closed ;' and GEN before, ': LATE ( n -- n ) MADE drop s" x" 1 die ; : E ( n -- n ) LATE 1 + ;' passes the check, and the load rejects E-DEAD-CODE at 4:21; so does a body left at a catch that never returns: ': LATE ( n -- n ) MADE ; : DD ( n -- ) LATE drop s" x" 1 die ; : C9 ( n -- n n ) [: DD ;] catch ; : E ( n -- n ) C9 drop ;' (load E-DEAD-CODE at C9 in E). Acceptance: the check and the load agree on all three programs: none rejected by the check alone, and a caller with code after a call of a left body that may never return is W-CHECK-DEFERRED at that call; diag-position cases. Depends: none. Claim: unassigned.
