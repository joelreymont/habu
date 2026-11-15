---
title: Let fcntl, pipe and poll failures report errno
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T00:01:43.617844+02:00"
---

Problem: the engine's fcntl and pipe primitives (src/habu/habu1.f:634, :706) answer -1 and set neither FS:ERRNO nor libc errno, so FD's FLAGS, ARM, RESTORE, PIPE-HELD and TRY-FIFO-WRITER's F_SETFL failure cannot report a cause, and WAIT-OUT has poll's errno as -rc but drops it. Found by the FD errno lane. Acceptance: each of those failures leaves its errno in FS:ERRNO, as fd.f's other failures do. Depends: none. Claim: unassigned.
