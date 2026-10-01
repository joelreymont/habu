---
title: "Report a linked child's deadline as a timeout"
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T11:32:11.971209+02:00"
---

Problem: test/preloaded-engine.f:111 LINKER-LOAD dies with the linked child's exit code. When a GE deadline fires inside gate-aot-negative or stripped-address, the child exits 67 (uncaught E-PROC-TIMEOUT) and the row reads kind=exit code=67, not TIMEOUT-UNDER-LOAD (measured with GE-TIMEOUT-MS forced to 1, ~/.cache/tmp/kestrel-r4-wblabel/log/r4-linked-1ms.log). Acceptance: the linked child ends a deadline with PROC-TIMEOUT-RC (the convention of test/aot-wid-build.f CHILD and test/proc-pty.f T-EXIT=) and LINKER-LOAD rethrows E-PROC-TIMEOUT for it, so the pool labels the row TIMEOUT-UNDER-LOAD; a real failure keeps its status. Prove with a forced 1 ms deadline before and after through test/gate-pool.f. Files: test/preloaded-engine.f and the linked children's entry.
