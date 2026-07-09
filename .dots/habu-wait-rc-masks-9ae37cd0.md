---
title: wait-rc masks signal deaths as rc 0
status: open
priority: 2
issue-type: task
created-at: "2026-07-05T00:00:31.440913+02:00"
---

src/habu/habu1.f BWAITRC ('wait-rc' primitive) returns WEXITSTATUS only: '9 9 8 LSRI, 9 9 $FF ANDI,'. For a signal-killed child (e.g. SIGABRT crash, raw status 0x86) WEXITSTATUS is 0, so wait-rc reports rc 0 for a CRASHED child - swallowed failure. lib/process.f PROC-WAIT-RC is correct (wait-status + PROC-STATUS>RC maps signals to 128+sig) but PROC-WAIT-RAW and tools/bench.f (line ~124 'PID @ wait-rc') still use the masking primitive. Fix: make BWAITRC report 128+termsig when the status term bits are set (matching PROC-STATUS>RC), or migrate the remaining wait-rc users to wait-status and retire the primitive; add a regression that spawns a self-killing child and asserts the reported rc is 128+sig, not 0. Found while fixing habu-install-force-exits-09c3c981.

## Library/tool side done (2026-07-06); engine prim retirement routed

Took the migrate-and-retire option. Landed:
- tools/bench.f RUN-HB: `PID @ wait-rc` -> `PID @ >PID PROC-WAIT-RC RC>N`
  (requires lib/errors.f + lib/process.f added); a signal-killed bench child
  now reports `FAILED rc=128+sig` instead of silently passing as rc 0.
- lib/process.f PROC-WAIT-RAW (the raw wait-rc alias, zero callers) retired:
  word deleted, lib/std.manifest row removed, docs/stdlib.md updated with an
  explicit "no raw wait-rc wrapper" rationale.
- Regression lib/process-test.f TEST-PROC-WAIT-RC-SIGNAL: /bin/sh child
  SIGKILLs itself; PROC-WAIT-RC must report 137 (128+9), never 0.
- Zero `wait-rc` call sites remain outside the engine (registration
  habu1.f:1831, PES row checker.f:3820, census noexec list entry).

REMAINING (engine-owned, routed): delete the BWAITRC primitive + its
`s" wait-rc"` registration (src/habu/habu1.f:1831), the `PRIM: wait-rc` PES row
(src/core/checker.f:3820), and the `wait-rc` name in the census noexec list
(test/prop-test-core.f AX-NOEXEC-B) in one engine change, keeping the
byte-for-byte fixpoint green. No stage0 mirror row exists (bootstrap/cg/forth.fs
has no wait-rc).
