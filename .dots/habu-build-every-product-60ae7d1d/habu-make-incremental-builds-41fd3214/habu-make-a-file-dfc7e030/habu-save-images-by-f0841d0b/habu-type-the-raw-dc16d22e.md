---
title: Type the raw records that hold addresses
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T22:20:06.022276+02:00"
closed-at: "2026-11-04T11:48:47.921647+02:00"
close-reason: "Census 2026-11-04: goes with habu-make-a-file-dfc7e030, the cross-build whose product is data (docs/design.md \"Machinery being replaced\"). TCB/Q-RECS/CB-HEADER typed so capture can tell addresses; runtime records never enter a product written as data."
---

T5 (plan section T5): task.f TCB (98-129, 1951-1959) and FACILITY (2026-2029), queue.f:49-54 Q-RECS, codegen.f:20-52 CB-HEADER become typed storage; integers holding addresses (TCB.THREAD, DBASE, CP, chain links) stay the owner's to clear before capture, as TASK-RELEASE-MEM does. Depends: T2. Claim: unassigned.
