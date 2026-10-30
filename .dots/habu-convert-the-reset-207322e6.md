---
title: Convert the reset spin locks to TASK facilities
status: open
priority: 3
issue-type: task
created-at: "2026-10-30T16:06:33.080089+02:00"
---

Problem: four CAS spin locks keep their own fork resets instead of TASK:FACILITY: src/lib/zip-state.f:38-49 STATE-LOCK (the file already requires task.f and fork-child.f), src/lib/fs-mutate.f:68 and :152-154 CLEANUP-LOCK, src/lib/process-tree.f:31 and :423-426 WALKING, src/lib/task.f:816 TASK-EXIT-LOCK. They do not hang a child (each has a reset) but duplicate the facility machinery and spin under contention. Found by the timer lane 5 review, 2026-10-30. Acceptance: each lock whose file may require src/lib/task.f becomes a TASK:FACILITY with FACILITY-INIT in its existing reset; any that stays (TASK-EXIT-LOCK is task.f's own exit-chain lock) says why in one sentence beside it. Files: those four. Verify: rg -n 'atomic-cas' src/lib shows only the kept ones; their fork tests pass on a rebuilt engine. Depends: timer lane 5 (the facility chain reset). Ownership: process-wide locks. Claim: unassigned.
