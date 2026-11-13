---
title: "Make TASK:KILL wait out a concurrent JOIN"
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T10:15:06.411546+02:00"
---

TASK-KILL (src/lib/task.f:1444) ignores TCB.JOINER, so a KILL racing a JOIN of the same task (TASK-JOIN-CHECK, :1462) releases the task under the joiner, and TASK:FREE (:2011) may then hand the record out again, so the joiner reads a freed or reused record. Decide the contract from the code (KILL refuses or waits for an active joiner), fix it in task.f, and add an e2e race test in src/lib/task-test.f. Claim: unassigned.
