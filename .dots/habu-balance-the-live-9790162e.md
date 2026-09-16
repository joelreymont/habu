---
title: Balance the live-task count across activating and joining threads
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T01:20:14.372245+03:00"
---

Problem: lib/task.f ACTIVATE increments the live-task counter of the ACTIVATING thread's region and TASK-JOIN-RELEASE (like KILL) decrements the JOINING thread's; when one task activates a worker and another joins it, the activating region's count never returns to zero and that region's later compilation is refused with the live-task guard ($4F). Found by habu-return-a-typed-b1c342cd, which makes the combination plausible for the first time; its tests deliberately join in the activating thread. Acceptance: the counter is process-wide or is charged and released against the worker's creator identity recorded in the TCB, so activate-here-join-there leaves every region at zero; a test activates in one task and joins (and kills) in another and then compiles at top level. Files: lib/task.f, lib/task-test.f, docs/threads.md. Verify: lib/task-test.f and test/run.f green on a rebuilt engine. Depends: habu-return-a-typed-b1c342cd. Ownership: lib/task.f. Claim: unassigned.
