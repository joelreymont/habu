---
title: "Nothing stores payload mode 2, and INTERN-SOURCE's origin is always -1"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T08:09:25.724740+02:00"
---

Problem (2026-11-11, by reading): once AOT-ARM:PAYLOAD-PERSISTENT is deleted no word stores 2 into AOT-ARM:PAYLOAD-MODE (src/habu/aot-arm.f:42), so src/habu/aot-capture.f ACAP-CHECKER-STRIP (:2261, always exits; its by-name lookup of CHECKER-SWEEP:STRIP, src/core/checker.f:32401, is then dead), the mode-2 branch of CAPTURE-COMPLETE (:2392, ACAP-COPY-CHECKER-DATA) and PAYLOAD-CAPTURE's path after the mode-1 exit (:2327) cannot run. INTERN:INTERN-SOURCE (src/habu/intern.f) always answers -1 as its origin, so the origin its callers file (src/compiler/native/elaborate.f:981 :1009 :3125 :4314, fetch.f:97, src/habu/t0.f:1257 :1262 :1348 :1375) carries nothing. Acceptance: each branch is traced to no live path and deleted with the words only it calls, or kept with the caller named; INTERN-SOURCE's callers call INTERN:STRING and the origin column goes if nothing reads a value other than -1 (t0.f is jerry's: advise, do not edit). Files: as listed. Verify: chain check; suite once at the landing. Depends: the INTERN removal landing. Ownership: unassigned. Claim: unassigned.

Found by the INTERN removal's review (2026-11-11, by reading; bare uses not searched), same acceptance: AOT-ARM:WINDOW-OPEN-PERSISTENT (src/habu/aot-arm.f:129-131) has no caller and worked only with PAYLOAD-PERSISTENT. SIG-CLOSE's `PAYLOAD-MODE @ 1 = if` (aot-arm.f:89) is always true where it is reached.
