---
title: TASK-AT-EXIT registers on a task without checking its claim
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T12:47:37.120934+02:00"
---

Problem: src/lib/task.f TASK:AT-EXIT - it does not check the task's claim, so registering on a task while TASK:FREE runs on it would write into freed storage. Read by the exit-head worker, not run. Source: Joel, 2026-11-10: "i want all issues identified to be dotted and fixed". Acceptance: a reproducer shows the race or the code shows it cannot happen; if it can, a registration on a task being freed is refused or ordered by the lock FREE takes. Files: src/lib/task.f, src/lib/task-test.f. Verify: the reproducer before and after. Depends: habu-the-exit-head-e1cea099 landed (same words). Ownership: task exit rows. Claim: unassigned.
