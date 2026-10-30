---
title: Move the remaining test waits onto the worker-end wait
status: closed
priority: 2
issue-type: task
created-at: "2026-10-30T20:47:21.677125+02:00"
closed-at: "2026-10-30T22:40:54.020925+02:00"
close-reason: "landed kpuzyvmk 2026-10-30: every listed wait uses TEST-WAIT:FOR-CELL; task-test, memory-test, fs-test rc 0 on bin/hb; planted -7353 in BIND-RACE-BINDER rc 67 with the code; planted throws in both fs readers F96/F97 -7353 rc 1 in 0.28 s; re-review ACCEPT after one comment line cut"
---

Problem: census B (/tmp/jerry-big/inproc-census-b.md section 1a) lists waits whose setter can throw first: src/lib/memory-test.f:632 and :605 (semaphore), src/lib/fs-test.f:777, src/lib/task-test.f:602 and :613, src/lib/zip-lifecycle-test.f:148, and the flag-only helpers TASK-WAIT-READY, APP-WAIT-CELL and Q-WAIT-CELL (queue-test.f). Acceptance: each uses TEST-WAIT:FOR-CELL (src/lib/test/wait.f, master 02e7a9f4); a semaphore wait whose poster can throw posts in a finally. Files: those five test files. Verify: each file green on bin/hb; a planted throw in one setter ends the case at once. Depends: none (FOR-CELL landed). Ownership: those files. Claim: unassigned.

Workspace: .jj-ws.noindex/jerry-waits (kpuzyvmk on ff4b6876), worker launched 2026-10-30 21:45, brief /tmp/jerry-big/waits-brief.md. Claim: jerry.

Lead edit EXIT-HALT-SENT (round 2 could set PAST before the second HALT was sent): task-test rc 0, test: ok, 4.93 s real 3.54 s user; a planted -7353 before the PAST store ends in 0.61 s with no process left. Review started 22:05; stopped by the usage limit, to resume.
