---
title: Keep aio-uring free of SIGPIPE on a broken pipe
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T14:25:48.879103+03:00"
---

Problem: on spark (aarch64 Ubuntu 24.04, kernel 6.17.0-1022-nvidia) `bin/hb --load lib/aio-test.f` exits 141 (SIGPIPE) in CASE-XFER-EMPTY-BROKEN: after close() of the pipe's read end the io_uring write runs inline in the submitting io_uring_enter and the kernel sends SIGPIPE to the process (SI_USER, si_pid = self); the test expects no SIGPIPE. Measured 8 of 8 standalone runs with the master engine 11c00585 (login and non-login shells; stdin /dev/null, a file or an empty pipe; strace in ~/.cache/habu-krait/habu-fail-closed-on-f84f1197/aio.strace on spark). The same row passed in the full gate at 5985bcb4 earlier the same day and in one concurrent gate run, so it is load- or timing-dependent. Acceptance: the aio-uring gate row passes on spark repeatedly; either the library prevents SIGPIPE for an io_uring write to a broken pipe (e.g. MSG_NOSIGNAL-equivalent or a blocked/ignored SIGPIPE scoped to the submission) or the test states the kernel behaviour it accepts, whichever matches lib/aio.f's documented contract. Files: lib/aio.f or lib/aio-test.f. Verify: spark, lib/aio-test.f 20 consecutive runs rc 0; gate. Ownership: unassigned (AIO library, not the Intel lane). Claim: unassigned.
