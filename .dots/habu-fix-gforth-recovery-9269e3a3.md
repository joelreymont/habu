---
title: Fix gforth recovery broken at current tip
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T16:31:12.700314+02:00"
---

Discovered 2026-07-21 during the boot-compat healing: HABU_ALLOW_BOOTSTRAP=1 tools/bootstrap.sh FAILS at current master - gforth aborts in test/bootstrap-wide-memory.fs:12 (c-abort backtrace through the 0.7.9 kernel). The no-binary recovery path is therefore broken exactly when it would be needed. The gforth install itself is healthy (verified end-to-end green this morning at an older commit), so this is a source regression in the recovery leg - most likely from the recent structure-make/certify-cycle landings touching bootstrap/cg or the wide-memory bootstrap test. Bisect the recovery run between the last-known-good commit and tip, fix at the root (the recovery leg must track the boot-file additions), and re-prove bootstrap.sh end-to-end to the byte-fixpoint. This is disaster-recovery infrastructure: treat as P1.
