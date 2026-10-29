---
title: "Make TASK:KILL wait out a concurrent JOIN"
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T10:15:06.411546+02:00"
---

TASK-KILL (src/lib/task.f) ignores TCB.JOINER, so a KILL racing a JOIN of the same task releases the record under the joiner. Once TASK:FREE and record reuse exist (phil's wyskoqmq), the joiner then reads a freed or reused record. Pre-existing; found by the Fable review of the TASK:NEW pair. Decide the contract from the code (KILL refuses or waits for an active joiner), fix it in task.f, and add an e2e race row in src/lib/task-test.f. Claim: unassigned.
