---
title: "The exit head's atomic ordering orders nothing the lock does not"
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T11:14:29.182421+02:00"
---

Problem: after habu-a-cleanup-registered-e2416748 every read of TCB.EXIT-SLOT holds TASK-EXIT-LOCK (src/lib/task.f:886, 902, 927, 939, 1441 at c1df1177) or runs in the capture (:1352), so the atomic store and acquire load on the head, and the comments that explain them (:814-815, :1420-1422), order nothing the lock does not already order. Found by that lane's review. Source: Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: the head is read and written as a plain cell under the lock and the two comments say the lock is the order; or a reader outside the lock is shown and the comments name it. Files: src/lib/task.f. Verify: src/lib/task-test.f; a run of the review's race case (/tmp/bob-review-exit/race.f, 2000 endings). Depends: habu-a-cleanup-registered-e2416748 landed. Ownership: TASK's exit chain. Claim: unassigned.

Claim: agent=bob workspace=.jj-ws.noindex/habu-the-exit-head-e1cea099
