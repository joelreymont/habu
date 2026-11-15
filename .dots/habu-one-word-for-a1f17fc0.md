---
title: One word for the time left before a deadline
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T10:26:42.429984+02:00"
---

Seven copies turn a mono-ns deadline into the milliseconds a poll waits: src/lib/process.f PROC-LEFT-MS, src/lib/pg.f LEFT-MS, src/lib/net/tcp4.f LEFT-MS, src/lib/net/ws.f REMAINING, src/lib/net/http-request.f REMAINING, and inline in src/lib/fd.f WAIT-OUT and src/lib/aio-macos.f. Two of them (PROC-LEFT-MS, tcp4 LEFT-MS) truncated, so a poll woke up to a millisecond before its deadline: in the runner test's signal child 3 of 60 runs failed (WAIT at ~8010 ms instead of 5000); a 3 ms window woke early in ~75% of rounds (lane jerry-poll rounds both up). About 30 files define their own 1000000 constant NS-PER-MS. One word at the module all of them can require, with one test, removes the copies. Workspace: none yet.

No test can show tcp4's private LEFT-MS ending early: a scratch T-SLICE-EMPTY bound at TCP4:SEND-SLICE-MS failed only 1 of 5 runs with truncation. The shared word's test (as process-test TEST-WAIT-REACHES-DEADLINE) covers every caller once the copies go.
