---
title: "Move lib/process.f's polls onto the AIO loop"
status: closed
priority: 3
issue-type: task
created-at: "2026-09-22T04:19:08.514808+03:00"
closed-at: "2026-09-30T17:53:07.000000+02:00"
close-reason: "dropped: POLL-IN and POLL-IN-OR-TIMEOUT have no production caller. Only test harnesses call them (lib/pty-harness.f, the process and gate tests, tests in Tender and Loom), from a main thread with nothing parked beside it, and by this dot's own scope the capture loop under every child run stays on poll(2)."
---

Problem: lib/process.f parks threads in poll(2) two ways: the single-descriptor POLL-IN / POLL-IN-OR-TIMEOUT that lib/pty-harness.f (READ-STEP, LEFTOVER-STEP, WATCH-READY?), test/proc-pty.f, test/signal-stub.f, lib/process-task-test.f, test/gate-env-stdin-tty-test.f, test/gate-pool-orphan-test.f and lib/process-test.f borrow, and the capture loop's PROC-POLL-CAPTURE / PROC-POLL-IO (nfds 2-3 over the PROC-PFD rows, PROC-POLL-ONCE / RESTART) under every child run. Scope: only the single-descriptor family moves, to POLL-ADD + AWAIT with the revents mapping the borrowers use, each borrowing program starting the loop after its last definition. The capture loop stays on poll(2) and keeps the PROC-PFD rows: a wait on the AIO loop needs the loop task live, a live task forbids compilation (docs/threads.md), and the programs that capture children compile afterwards - test/gate-common-lib.f evaluates source (GE-EVAL-SOURCE-ACT) in the suites that also run children, tools/build-fixpoint.f evaluates text (BF-EVAL-N) - so a capture that required the loop would break the gate and the builder. Acceptance: rg -n 'POLL-IN' lib test finds only comments; PROC-PFD and PROC-POLL-* remain for the capture loop, with a comment on PROC-POLL-CAPTURE saying why; the borrowing suites pass with LOOP-START; docs/threads.md's process.f row. Depends: habu-move-the-poll-6fad96da (closed). Ownership: hazel.
