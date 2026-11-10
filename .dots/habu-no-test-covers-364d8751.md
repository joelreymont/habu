---
title: "No test covers the capture's sweep of exit rows"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T11:10:15.022200+02:00"
---

Problem: nothing tests EXIT-ROWS-SWEEP (src/lib/task.f, the capture's sweep of AT-EXIT rows into the table; `rg -n 'AT-EXIT cleanups at capture' test tools src` finds no test). habu-a-cleanup-registered-e2416748 makes rows three cells with an OWED mark the sweep must clear; a sweep that carried a row owed, or kept a two-cell stride, would pass every test. Found by that lane's worker. Source: Joel, 2026-11-10: "you must dot and launch a fix automatically for any issues you identify". Acceptance: one end-to-end case: a program registers a cleanup, is captured, and the captured image's task runs the cleanup exactly once at its ending; stated beside it why a capture is the boundary under test. Files: the capture test that owns AT-EXIT (locate), src/lib/task-test.f if no capture test fits. Verify: the case on bin/hb, failing against a sweep that leaves the mark set. Depends: habu-a-cleanup-registered-e2416748 landed. Ownership: TASK's tests. Claim: unassigned.
