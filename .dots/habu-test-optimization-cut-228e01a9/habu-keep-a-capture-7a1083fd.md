---
title: "Keep a capture's own code when its tree walk throws"
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T04:14:46.991384+02:00"
---

With f2cf1e39, lib/process.f PROC-KILL-CAPTURE runs PROC-TREE:KILL-TREE inside finally at every early end of a capture. A walk that throws (E-PROC-TRUNCATED past 1024 members, E-PROC-OUTPUT, E-PROC-HOST) rethrows through PROC-REAP-CAPTURE-TIMEOUT and PROC-THROW-CAPTURE before PROC-TIMED-OUT is set or the capture's own code is thrown, so the caller sees the walk's code: tools/check.f reports a deadline over a 1025-process tree as the output cap (CHK-RUN-TOO-BIG) and other walk errors as an uncaught throw (67). Fix in lib/process.f: the child is still killed and reaped, the capture's own code reaches the caller, and the walk's failure is not swallowed. Found by review 387.
