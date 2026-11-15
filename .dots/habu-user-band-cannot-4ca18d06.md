---
title: "A program can store into the task area base"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T11:28:08.578531+02:00"
---

Problem: task.f's area base (TASK-AREA-OFF, src/lib/task.f:184) sits in USER-BAND, where a program's store is accepted, so a program that stores there makes every slot word answer an address it chose (habu-checked-src-reads-4f721c9b covers data-base plus an offset).
Acceptance: the area-base cell is out of reach of a program's store, as MEM-ABI:HEAD-OFF is; tasking-threads and the process suites pass; Maki, Zen and Tender build.
Depends: none. Claim: unassigned.
