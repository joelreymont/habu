---
title: "A task's region lacks the engine's callback cells"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:43:15.541822+02:00"
---

Problem: src/lib/task.f TASK-REGION-INIT (:1274) copies a fixed list of DATA cells into a task's zeroed region; EVALUATE-CELL, COMP-CELL and CREATEP-CELL are not on it. On the installed Habu ee4b1361 `included` inside a TASK:TASK ends with rc 79 and no message (seen by the row-walk review, cause not diagnosed; whether evaluate in a task jumps to 0 is unchecked). Acceptance: the cause of rc 79 is known; a task either runs included and evaluate as the main thread does or refuses them with a message; every callback cell a task can reach is copied or its absence is refused. Files: src/lib/task.f, src/habu/layout.f (the callback cells). Verify: a task case for included and evaluate in src/lib/task-test.f. Depends: none. Ownership: src/lib/task.f TASK-REGION-INIT. Claim: unassigned.
