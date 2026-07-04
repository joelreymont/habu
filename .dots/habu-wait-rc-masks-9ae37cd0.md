---
title: wait-rc masks signal deaths as rc 0
status: open
priority: 2
issue-type: task
created-at: "2026-07-05T00:00:31.440913+02:00"
---

src/habu/habu1.f BWAITRC ('wait-rc' primitive) returns WEXITSTATUS only: '9 9 8 LSRI, 9 9 $FF ANDI,'. For a signal-killed child (e.g. SIGABRT crash, raw status 0x86) WEXITSTATUS is 0, so wait-rc reports rc 0 for a CRASHED child - swallowed failure. lib/process.f PROC-WAIT-RC is correct (wait-status + PROC-STATUS>RC maps signals to 128+sig) but PROC-WAIT-RAW and tools/bench.f (line ~124 'PID @ wait-rc') still use the masking primitive. Fix: make BWAITRC report 128+termsig when the status term bits are set (matching PROC-STATUS>RC), or migrate the remaining wait-rc users to wait-status and retire the primitive; add a regression that spawns a self-killing child and asserts the reported rc is 128+sig, not 0. Found while fixing habu-install-force-exits-09c3c981.
