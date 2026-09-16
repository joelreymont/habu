---
title: Return errno from poll so capture survives signals
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:32:15.278827+03:00"
---

Problem: src/habu/habu1.f BPOLL collapses every poll(2) error to -1 (`0 0 MOVN`), so lib/process.f PROC-POLL-CAPTURE cannot tell EINTR from EBADF and raises E-PROC-OUTPUT on any signal; poll is never restarted by SA_RESTART. Measured 2026-09-16 (profiler lane): with the 1 kHz sampling profiler armed, poll returns -1 after 1 ms instead of rc 0 after 2001 ms, and `include tools/native-build.f` dies at its final child-spawn phase with E-PROC-OUTPUT after about 99 s. Any signal breaks child capture, not only the profiler; lib/process-fork.f and lib/serial.f already loop over EINTR, the capture path does not. Acceptance: BPOLL (and the sibling syscall primitives that collapse errno the same way: audit read/write/wait/accept in habu1.f and the Gforth mirror) return -errno; PROC-POLL-CAPTURE and every caller that tests `rc 0 <` retry on EINTR and name any other errno through the existing E-PROC-* codes; a regression arms a timer signal at 1 kHz and captures a child that runs two seconds; the profiler lane's sample limit workaround is removed and the self-build profiles end to end; Gforth mirror in step (two-stage rule if the primitive ABI changes); full gate green. Files: src/habu/habu1.f (BPOLL and siblings), src/core/checker.f (PRIM rows if the effect changes), lib/process.f, lib/process-fork.f, lib/serial.f, bootstrap/cg/forth.fs, test/. Verify: the regression; test/process-*.f, test/task-*.f, lib/serial-test.f; tools/native-build.f fixpoint; tools/bootstrap.sh; test/run.f. Depends: none. Ownership: OS seam primitives. Claim: unassigned. Source: profiler worker 2026-09-16.
