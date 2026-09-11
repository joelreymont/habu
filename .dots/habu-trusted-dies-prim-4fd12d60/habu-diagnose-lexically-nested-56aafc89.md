---
title: Diagnose lexically nested quotation elaboration rejection
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-11T16:51:00.123952+03:00\""
closed-at: "2026-09-11T17:21:56.229628+03:00"
close-reason: Reviewed a301eb61 integrated as ebe19a15; combined explicit-tier1 quotation suite passes original nested reducer, deeper/loop/EXIT/sibling cases and capture/unknown-effect negatives. JIT counterpart remains7c5267cd.
---

Unassigned; discovered by /root/check_api while testing quotation-frame repair 5137130d on cfcf1baf. Exploratory reducer `[: >r [: >r RV:CALLEE r> + ;] execute r> + ;]` rejects -8651 during elaboration before allocation. This is distinct from spills inside quotation function bodies: runtime nesting via a named callee passes after that frame fix. Acceptance: record full typed enclosing definition and inputs, reduce and determine whether declaration or compiler is wrong, fix responsible layer if valid, and add a real load-path regression without broadening frame repair. Current evidence is the agent's exploratory report, not an independently reproduced root diagnosis.


Update 2026-09-11 13:59 UTC: Owner now /root/check_api, workspace cedar-nested-quotations on 035bfb4c. Complete reducer: RV:CALLEE ( n -- n ) is 1+; `: QBN ( n n n -- n ) [: >r [: >r RV:CALLEE r> + ;] execute r> + ;] execute ;` with 3 7 11 must return 22. CHECK! certifies it (-1), no captured locals. AOT QSCAN-STEP explicitly rejects a second [: while QD is open (-8651). Agent owns QSCAN/QOPEN/QCLOSE, arity finalization and QBUILD ordering plus native-quot regression; no NO-REAL-CK overlap. JIT also imposes one open [: limit (rc75), to track as a separate tier limitation.


Update2026-09-11 14:15 UTC: Fixed by a301eb61, root independent actual-diff review clear, integrated as ebe19a15. Opening-token ownership supplies lexical parent; body pre-order establishes child ABI before each QBUILD. Explicit tier1 native-quot5.312s, quot-scope4.297s, rstack4.501s, order-exit0.423s pass; original3/7/11 returns22, with deeper nesting, loops, innerEXIT, sibling identity and capture/unknown-effect negatives. Matched cb6304354c00216a04bd9e7dac7e2061c380db77070e3f35c9283737303aeef0. JIT counterpart separately assigned to Rowan, habu-support-nested-quotations-7c5267cd.
