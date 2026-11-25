---
title: Say that a quotation may never return
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T20:46:50.566573+01:00"
---

Problem: a quotation literal whose body ends on a call that may never return is taken as returning (src/core/checker.f:22816, `PATH-DEAD? XSET @ 0= and {: noret:bool :}`), so the check and the load disagree. With LATE a left body that may never return: ': Q ( -- n ) [: LATE ;] execute 1 ;' loads with E-DEAD-CODE at 4:33 and passes the check; ': Q2 ( -- n ) [: LATE ;] execute ;' loads and the check says E-MISMATCH; the same through ': RUN2 ( R [ R -- S ] -- S )'. Acceptance: the quotation carries a may-never-return fact that `execute`, `catch` and a `[ R -- S ]` callee read; the check and the load agree on all three programs, and a caller with code after such a run is W-CHECK-DEFERRED there; diag-position cases. Depends: none. Claim: unassigned.
