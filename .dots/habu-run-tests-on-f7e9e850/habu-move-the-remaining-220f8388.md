---
title: Move the remaining test waits onto the worker-end wait
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T20:47:21.677125+02:00"
---

Problem: census B (/tmp/jerry-big/inproc-census-b.md section 1a) lists waits whose setter can throw first: src/lib/memory-test.f:632 and :605 (semaphore), src/lib/fs-test.f:777, src/lib/task-test.f:602 and :613, src/lib/zip-lifecycle-test.f:148, and the flag-only helpers TASK-WAIT-READY, APP-WAIT-CELL and Q-WAIT-CELL (queue-test.f). Acceptance: each uses TEST-WAIT:FOR-CELL (src/lib/test/wait.f, master 02e7a9f4); a semaphore wait whose poster can throw posts in a finally. Files: those five test files. Verify: each file green on bin/hb; a planted throw in one setter ends the case at once. Depends: none (FOR-CELL landed). Ownership: those files. Claim: unassigned.
