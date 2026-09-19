---
title: Restart pty one-shot polls on EINTR against a deadline
status: active
priority: 2
issue-type: task
created-at: "2026-09-16T17:19:47.591400+03:00"
---

Problem: after a0c78d5e (poll returns -errno and the capture pollers restart on -EINTR against their deadline), lib/process-pty-io.f IO-POLL-READY? and AWAIT-BYTES and lib/pty.f READ still throw on any negative poll rc, -EINTR included: they are one-shot polls that own no deadline to restart against, so a profiled pty capture (or any signal at the wrong moment) still dies (poll lane audit, 2026-09-16, untested boundary 5). Acceptance: the three words take or derive a deadline and restart through lib/process.f PROC-POLL-RESTART like the capture pollers; a regression arms the profiler at 1 kHz around a pty session (test/process-pty-io-smoke.f shape) and asserts the output is intact; no other pty behaviour changes; lib/pty-test.f, test/process-pty-*.f green. Also record the seed divergence the audit found: bootstrap/cg/forth.fs BREAD/BWRITE publish raw x0 (-errno) while habu1.f collapses to -1; decide one contract for read/write at the primitive (the BPOLL comment states the rule) and mirror it. Files: lib/process-pty-io.f, lib/pty.f, lib/process.f, src/habu/habu1.f (read/write if the contract changes), bootstrap/cg/forth.fs, test/. Verify: the regression; lib/pty-test.f; test/process-pty-io-smoke.f; test/process-pty-tty-smoke.f; test/run.f. Depends: none. Ownership: pty library and the OS seam. Claim: unassigned.

Claim: alder, .jj-ws/alder-pty-eintr on 27219507. Hazel released the library
PTY/process side, explicitly no engine edits. Keep the seed read/write errno
divergence recorded above outside this library slice. All three words still
call raw poll and throw on -EINTR. Reuse the process-owned task-local poll row
and PROC-POLL-RESTART, preserving timeout, readiness and error classification.

Measured red before: unchanged libraries with the new profiler fixtures throw
E-PTY-IO (-9162) at PTY:READ and E-PROC-OUTPUT (-2503) at each of AWAIT and
AWAIT-BYTES. The byte-wait reduction skips only the preceding watch test in a
scratch copy so its independent failure can be seen. Logs:
/tmp/alder-pty-eintr/before-{pty,supervisor,output}.log.

Fix: all three waits use one monotonic entry deadline with PROC-POLL-RESTART
and the existing task-local pollfd row; duplicate static poll buffers and their
packing helpers are removed. The shared retry keeps negative initial timeouts
unbounded, preserving the old raw-poll behavior. Fixtures pin 100 ms quiet
deadlines under 1 kHz profiler signals and complete terminal output under an
unbounded interrupted wait.

All 28 focused registry rows pass on private native host 31be3fb0, including
pty, both process-pty rows, pty-harness, proc-capture-under-signals,
process-signals, process-image and every tools/*-test.f reader's row except
build-fixpoint-fixtures (its install --force is prohibited in this lane).
Full logs: /tmp/alder-pty-eintr/logs. Astra review clear; no engine edits or
full gate, no macOS execution. Hazel owns the serial gate and the remaining
seed errno decision; this commit addresses the released library slice only.
