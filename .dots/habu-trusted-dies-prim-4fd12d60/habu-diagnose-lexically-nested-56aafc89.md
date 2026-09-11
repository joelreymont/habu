---
title: Diagnose lexically nested quotation elaboration rejection
status: open
priority: 1
issue-type: task
created-at: "2026-09-11T16:51:00.123952+03:00"
---

Unassigned; discovered by /root/check_api while testing quotation-frame repair 5137130d on cfcf1baf. Exploratory reducer `[: >r [: >r RV:CALLEE r> + ;] execute r> + ;]` rejects -8651 during elaboration before allocation. This is distinct from spills inside quotation function bodies: runtime nesting via a named callee passes after that frame fix. Acceptance: record full typed enclosing definition and inputs, reduce and determine whether declaration or compiler is wrong, fix responsible layer if valid, and add a real load-path regression without broadening frame repair. Current evidence is the agent's exploratory report, not an independently reproduced root diagnosis.


Update 2026-09-11 13:59 UTC: Owner now /root/check_api, workspace cedar-nested-quotations on 035bfb4c. Complete reducer: RV:CALLEE ( n -- n ) is 1+; `: QBN ( n n n -- n ) [: >r [: >r RV:CALLEE r> + ;] execute r> + ;] execute ;` with 3 7 11 must return 22. CHECK! certifies it (-1), no captured locals. AOT QSCAN-STEP explicitly rejects a second [: while QD is open (-8651). Agent owns QSCAN/QOPEN/QCLOSE, arity finalization and QBUILD ordering plus native-quot regression; no NO-REAL-CK overlap. JIT also imposes one open [: limit (rc75), to track as a separate tier limitation.
