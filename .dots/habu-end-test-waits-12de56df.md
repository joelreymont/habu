---
title: End test waits on their event, not a deadline (timer lane 4)
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:34:19.456866+02:00"
---

Problem: test rendezvous wait on a deadline instead of the event they wait for, and the deadline expires under load. Recurring reproducer: test/db/pg-kill-test.f REPORT-READ? waits READY-MS 60 s (test/db/pg-kill-test.f:25, expiry at :98); under a loaded full suite the pg row fails 'pg-kill-test REPORT-READ: a deadline expired' (rc 67, throw -2502) and passes alone in 5 to 12 s; it was the only red in several full suites. On macOS the FIFO reports no end of file, so this wait needs another event, the row's exit, before its deadline can go. The other timed waits of this lane: src/lib/test/wait.f (POLL :23, WAIT-OPEN :172, WATCH-READY? :271, EXIT-READY? :281) and its users, among them src/lib/fs-list-test.f, src/lib/fs-mutate-test.f, src/lib/process-task-test.f, src/lib/fs-cause-test.f, src/lib/fs-rename-noreplace-test.f and test/db/pg-test.f. Joel's words: 'why do we need a test where we have to artificially restrict it in case it hangs?'. Acceptance: each of these waits ends on its event (a readable fd, a child's exit) and takes no ms; the pg row passes a full suite under load. Files: src/lib/test/wait.f, test/db/pg-kill-test.f, test/db/pg-cluster.f and the tests named above. Verify: rg -n 'READY-MS' test/db prints nothing; hb --load test/db/pg-kill-test.f rc 0 alone and in a full suite under load. Depends: none. Ownership: unassigned. Claim: unassigned.
