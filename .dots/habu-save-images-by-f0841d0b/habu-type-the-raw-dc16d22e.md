---
title: Type the raw records that hold addresses
status: open
priority: 2
issue-type: task
created-at: "2026-10-31T22:20:06.022276+02:00"
---

T5 (plan section T5): task.f TCB (98-129, 1951-1959) and FACILITY (2026-2029), queue.f:49-54 Q-RECS, codegen.f:20-52 CB-HEADER become typed storage; integers holding addresses (TCB.THREAD, DBASE, CP, chain links) stay the owner's to clear before capture, as TASK-RELEASE-MEM does. Depends: T2. Claim: unassigned.
