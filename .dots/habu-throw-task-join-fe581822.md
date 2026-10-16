---
title: Throw task-join and parked-wait deadlines
status: open
priority: 2
issue-type: task
created-at: "2026-10-16T15:11:46.413957+03:00"
---

Problem: docs/gate.md:428-441 says a test's own deadline is no assertion result: a wait whose clock ends throws E-PROC-TIMEOUT unless the test expects that deadline and asserts it. These bounded waits answer a value a later assert reads instead: src/lib/net/udp4-test.f REACHED? (:157-163) answers false at WAIT-MS, read by TTRUE (:239, :242 and its other callers), and PARK-WORK (:221-231) stores -2 on a receive timeout, read by 'PARK-GOT @ PAYLOAD-N T=' (:243); src/lib/signal-test.f JOIN-TASK (:328-334) returns at TASK-JOIN-MS and the cases assert TASK:DONE? TTRUE (:390 and the other JOIN-TASK callers); src/lib/serial-test.f JOIN-READER (:103-109) returns at JOIN-MS, read by 'READER TASK:DONE? TTRUE' (:185), and PARK-WORK (:174-175) records a read timeout as PARK-KIND, read by 'PARK-KIND @ K-MOVED T=' (:188); src/lib/task-test.f asserts TASK:DONE? TTRUE after APP-WAIT-DONE (:474, :1259, :1355, :1374), a deadline only if APP-WAIT-DONE is bounded. Found by the 8dcd9878 in-process lane. Acceptance: each bounded wait whose clock ends, where the case does not expect it, prints a line naming the wait and throws E-PROC-TIMEOUT; a wait that is unbounded or whose expiry the case expects stays, with the reason in the closing note; no deadline is raised. Files: the four tests. Verify: each changed row rc 0; per changed mechanism a copy with the deadline forced exits 67 with hb: uncaught throw code -2502 last. Depends: none. Ownership: those files. Claim: unassigned.
