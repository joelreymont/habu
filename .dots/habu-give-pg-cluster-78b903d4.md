---
title: Give pg-cluster its hold FIFO without the environment
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T22:34:19.515773+02:00"
---

Problem: test/db/pg-kill-test.f:141 sets the environment variable PG_HOLD_FIFO for the pg row, and test/db/pg-cluster.f:434-435 and test/db/pg-hold.f:70 read it to open the FIFO that test/db/pg-kill-test.f watches. A test child's setting can travel as a script argument or program text instead. Source, Joel 2026-10-29: remove env vars except where Habu really needs one and cannot do without it. Acceptance: pg-cluster and pg-hold receive the FIFO path from their arguments or program text, and PG_HOLD_FIFO is gone. Files: test/db/pg-kill-test.f, test/db/pg-cluster.f, test/db/pg-hold.f. Verify: rg -n PG_HOLD_FIFO prints nothing outside .dots; hb --load test/db/pg-kill-test.f rc 0. Depends: none. Ownership: unassigned. Claim: unassigned.
