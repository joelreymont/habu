---
title: "USER-BAND claims 5264 bytes for two cells"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T11:28:08.578531+02:00"
---

Problem: per-task slots live in the area mapped after each region's header (src/lib/task.f), yet USER-BAND (src/habu/layout.f:1183-1188, claimed at src/habu/data-claims.f:226) still spans TXN-STATE's end to MEM-ABI:START, about 5264 bytes, of which task.f uses two cells: PAUSE's depth (TASK-DEFER-OFF :178) and the area base (TASK-AREA-OFF :181). The area base sits where a program's store is accepted, so a program that stores there makes every slot word answer an address it chose (habu-checked-src-reads-4f721c9b covers data-base plus an offset).
Acceptance: USER-BAND is those two cells and no band claims the bytes it frees; the area-base cell is out of reach of a program's store, as MEM-ABI:HEAD-OFF is; tasking-threads and the process suites pass; Maki, Zen and Tender build.
Depends: none. Claim: unassigned.
