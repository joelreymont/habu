---
title: Store RUNNING conditionally in the task entry
status: open
priority: 3
issue-type: task
created-at: "2026-09-21T23:41:22.832029+03:00"
---

Problem: BTASK-ENTRY (src/habu/habu1.f:2079) stores TASK-ABI:RUNNING into the TCB status with a plain STR after ACTIVATE already stored it, so a TASK:HALT that lands between pthread_create and that store - its atomic-cas took RUNNING -> HALT-REQ, the stop flag is set, the park posted - is overwritten back to RUNNING. The halt still takes effect because PAUSE reads TCB.STOP, not the state, but the state cell lies until DONE and anything that waits on the state would miss the request (reported by the kill-race lane, 2026-09-21). Acceptance: the entry stores RUNNING only if the cell still holds RUNNING (a CAS or conditional store), or the store goes because ACTIVATE's is the one that counts; a regression in lib/task-test.f that HALTs inside the create window and reads HALT-REQ; three generations with gen2 == gen3. Files: src/habu/habu1.f, src/habu/task-abi.f, lib/task-test.f. Verify: lib/task-test.f, test/run.f. Depends: none. Ownership: hazel.
