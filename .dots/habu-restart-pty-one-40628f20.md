---
title: Restart pty one-shot polls on EINTR against a deadline
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T17:19:47.591400+03:00"
---

Problem: after a0c78d5e (poll returns -errno and the capture pollers restart on -EINTR against their deadline), lib/process-pty-io.f IO-POLL-READY? and AWAIT-BYTES and lib/pty.f READ still throw on any negative poll rc, -EINTR included: they are one-shot polls that own no deadline to restart against, so a profiled pty capture (or any signal at the wrong moment) still dies (poll lane audit, 2026-09-16, untested boundary 5). Acceptance: the three words take or derive a deadline and restart through lib/process.f PROC-POLL-RESTART like the capture pollers; a regression arms the profiler at 1 kHz around a pty session (test/process-pty-io-smoke.f shape) and asserts the output is intact; no other pty behaviour changes; lib/pty-test.f, test/process-pty-*.f green. Also record the seed divergence the audit found: bootstrap/cg/forth.fs BREAD/BWRITE publish raw x0 (-errno) while habu1.f collapses to -1; decide one contract for read/write at the primitive (the BPOLL comment states the rule) and mirror it. Files: lib/process-pty-io.f, lib/pty.f, lib/process.f, src/habu/habu1.f (read/write if the contract changes), bootstrap/cg/forth.fs, test/. Verify: the regression; lib/pty-test.f; test/process-pty-io-smoke.f; test/process-pty-tty-smoke.f; test/run.f. Depends: none. Ownership: pty library and the OS seam. Claim: unassigned.
