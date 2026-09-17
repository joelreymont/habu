---
title: "Keep KEY1's read uninterrupted when signal handlers land"
status: open
priority: 2
issue-type: task
created-at: "2026-09-17T17:59:24.910678+03:00"
---

Problem: src/habu/repl.f KEY1 (habu-make-key1-end-cabdb980, 2026-09-17) treats a one-byte read answering below 1 as end of input, and the engine collapses a failed read to -1 without errno (src/habu/habu1.f SYS-PUSH errno rule), so an EINTR would end the REPL line as if the terminal hung up. That is sound today because the only asynchronous handler the engine installs (the profiler's SIGALRM) sets SA_RESTART and the crash handlers exit, but aspen's lib/signal.f (in review) lets user programs install handlers that return. Acceptance: either every handler lib/signal.f installs carries SA_RESTART (documented as the rule in docs/ beside the library's contract, with a test that a handler returning during a blocked KEY1 read does not end the line), or KEY1 distinguishes an interrupted read from end of input; the choice is recorded where the errno rule lives. Files: lib/signal.f, src/habu/repl.f, src/habu/habu1.f (errno rule comment), test/. Verify: a pty test that raises a handled signal while the child REPL waits for a key and then completes the line. Depends: aspen's lib/signal.f landing. Ownership: signal library. Claim: unassigned.
