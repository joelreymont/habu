---
title: Load src/habu/debug.f on an x86-64 engine
status: closed
priority: 3
issue-type: task
created-at: "2026-11-01T20:05:12.037580+02:00"
closed-at: "2026-11-04T11:48:48.165018+02:00"
close-reason: "Census 2026-11-04: an acceptance case of habu-make-a-file-dfc7e030, written there. require debug.f on x86 hits E-SEALED-PACKAGE: a file built into hb must load cleanly; inferred, not probed."
---

On x86-64, 'require src/habu/debug.f' fails 'E-SEALED-PACKAGE: DEBUG at src/habu/debug.f:12', rc 84, at tier 0 and tier 1, on master efef8267's x86 engine and the T0 x86 stack alike (joel@pulsar-ua-106). It stops engine-runtime-regressions at GE-PROCESS-PTY (test/proc-pty.f requires debug.f). Found by C4b fold-in B (jerry-t0x86jit). Claim: unassigned.
