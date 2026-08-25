---
title: Fix the proc-pty pool-contention flake
status: open
priority: 2
issue-type: task
created-at: "2026-08-14T14:31:18.722021+02:00"
---

Seen at the widening merge gate: test/proc-pty.f red once under the gate pool (F10, the surface case - it printed the CORRECT fold then died on pty mechanics; the pool showed WAIT lines) and green standalone and on the immediate re-run of the identical tree. A pty case that flakes under pool contention will eventually red an honest merge. Find the timing hole (read timeout? pty teardown race?) and make the case robust or the failure attributed. Files: test/proc-pty.f, lib/process-pty*.f. Depends: none.

CONTEXT (gate-scan lane wrap-up 2026-08-14): aot-wid-suite's child
engine builds rose 4 to 7 in the standalone stdlib gate - pool
contention is INCREASING as the bake work adds boot fixtures, so
this flake's window widens. The gate-scan lane also saw
native-match killed 137 once under cross-lane contention. Price
the pool's contention envelope while fixing the pty case.
