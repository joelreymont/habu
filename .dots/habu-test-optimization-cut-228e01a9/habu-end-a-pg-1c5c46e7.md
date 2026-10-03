---
title: End a pg row deadline as a timeout
status: closed
priority: 2
issue-type: task
created-at: "\"2026-10-01T11:07:52.681773+02:00\""
closed-at: "2026-10-01T17:10:46.597269+02:00"
close-reason: Fixed by uqlpqsvr 18f0bd8c (review 104 ACCEPT)
---

Problem (r4-wblabel commit 2 report): test/db/pg-cluster.f ends every deadline as an ordinary failure: EXITED-0? prints 'passed its deadline' and INITDB-RUN dies 1 (:432-442), READY? 'not ready inside its deadline' returns false, RUN-FILE's case past FILE-MS (:458-465) returns false. The pool labels these kind=exit, not TIMEOUT-UNDER-LOAD, so a loaded host reads as a pg defect. A bare throw would skip START's STOP and leave postgres running. Acceptance: after the harness's own cleanup (STOP, socket dir), a deadline ends the row with an uncaught E-PROC-TIMEOUT so GT-POOL-INNER-TIMEOUT? labels it; real failures keep their status; docs/db.md says so. Forced 1 ms deadlines for initdb, ready and a case file through test/gate-pool.f show kind=exit before and TIMEOUT-UNDER-LOAD after, with ipcs -m unchanged. Files: test/db/pg-cluster.f, docs/db.md. Verify: test/db/pg-kill-test.f, the pg gate rows, test/gate-pool-test.f.
