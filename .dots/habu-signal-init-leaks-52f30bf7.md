---
title: "SIGNAL:INIT leaks its pipe when a nonblocking mark is refused"
status: open
priority: 2
issue-type: task
created-at: "2026-11-05T11:35:54.733396+02:00"
---

From the FD:PIPE lane review, 2026-11-05. Problem: src/lib/signal.f OPEN-PIPE marks both FD:PIPE ends with PROC-NONBLOCK! (process.f), which throws through PROC-THROW-CAPTURE: a refused fcntl leaves both ends open (never stored in READ-FD-CELL/WRITE-FD-CELL) and runs PROC-END-CAPTURE on the calling task's capture row, which SIGNAL:INIT never set up. Acceptance: a refused mark closes both ends and throws E-FS-IO (fd.f's code for a refused fcntl) without touching the capture row; signal.f no longer needs process.f's capture-row word for this. Files: src/lib/signal.f and src/lib/fd.f: FD:NONBLOCK! ( fd -- ), approved 2026-11-05 (DECISIONS "a non-blocking descriptor"), throws E-FS-IO when the mark is refused or not kept. Verify: bin/hb --load src/lib/signal-test.f on macOS and spark. Depends: habu-one-close-on-62de6ce7. Ownership: carl. Claim: unassigned.
