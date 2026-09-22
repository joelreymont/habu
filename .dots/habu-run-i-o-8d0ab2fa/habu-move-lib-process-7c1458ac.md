---
title: "Move lib/process.f's polls onto the AIO loop"
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T04:19:08.514808+03:00"
---

Problem: lib/process.f still parks threads in poll(2): PROC-POLL-CAPTURE/PROC-POLL-IO (nfds 2-3 over the PROC-PFD rows, PROC-POLL-ONCE/RESTART) and the single-fd POLL-IN / POLL-IN-OR-TIMEOUT, borrowed by lib/pty-harness.f (READ-STEP, LEFTOVER-STEP, WATCH-READY?), test/proc-pty.f, test/signal-stub.f, lib/process-task-test.f, test/gate-env-stdin-tty-test.f, test/gate-pool-orphan-test.f, lib/process-test.f. Design: the multi-fd capture polls become AIO:GROUP + AWAIT-ANY, the single-fd ones POLL-ADD + AWAIT with the revents mapping the six libraries used; the PROC-PFD rows and the engine poll primitive's callers retire where nothing else reads them; every caller starts the loop. Acceptance: rg -n 'PROC-PFD|PROC-POLL|POLL-IN' lib test finds only comments; the borrowing suites pass with LOOP-START; docs/threads.md's process.f row. Depends: habu-move-the-poll-6fad96da (closed). Ownership: hazel.
