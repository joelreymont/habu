---
title: Relocate native is dispatch-cell addresses through image capture
status: active
priority: 1
issue-type: task
created-at: "2026-09-13T21:50:04.179680+03:00"
---

Confirmed in private B1 from d7184652 (SHA256
a4fc1996d67ae9a46d53d9cea58622a31291fc5d3f5cf9e1de58f2ce07a6c85a):
`1 set-tier : ID ( n -- n ) 1 + ; 17 ID .` throws E-NFEED-STATE (-8400).
The scan owner and ARMED flag agree, but no installed observer events arrive.
Saved CHECKER-TAPE:INSTALL writes SCAN-XT to its build-time DATA address
0x34111e1c0; the relocated deferred word reads 0x340551320. A process-local JIT
replacement of only the owner installer makes the same image return 18.

DO-IS emitted the dispatch-cell pointer with HIR:ADDR-NONE, so AOT relocation
could not see it. The fix uses HIR:ADDR-DATA through EMIT-KIND-LIT.

Regression: `test/native-defer-image.f` first compiles with the candidate's
captured native compiler, then saves through APP-IMAGE:SAVE and reassigns defer
callbacks after two restores outside the checkout. The original B1 fails at its
first native definition with -8400. The current source compiler passes
`test/compiler/native-defer.f`, including its three rejected non-defer targets.
Fresh fixed image validation remains in progress with the integrating build.
