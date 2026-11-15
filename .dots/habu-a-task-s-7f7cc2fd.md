---
title: "A task's region lacks the engine's callback cells"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T20:43:15.541822+02:00"
---

Problem: TASK-REGION-INIT (src/lib/task.f:1122) copies a fixed list of DATA cells into a task's zeroed region; EVALUATE-CELL, COMP-CELL and CREATEP-CELL are not on it. On the installed Habu, `included` inside a TASK:TASK ends with rc 79 and no message. Read, not run: 79 is RC-TASK-THREAD (src/habu/interpreter.f:242), which TASK-GUARD (:2580-2584, called by RUN-TOKENS at :2607) raises through FAIL-CLOSED with an empty text on a task's thread. Acceptance: the cause of rc 79 is confirmed; a task either runs included and evaluate as the main thread does or refuses them with a message; every callback cell a task can reach is copied or its absence is refused; src/lib/task-test.f has a task case for included and evaluate. Depends: none. Claim: unassigned.
