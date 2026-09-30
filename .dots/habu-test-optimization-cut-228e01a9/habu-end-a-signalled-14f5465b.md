---
title: "End a signalled gate's children and scratch"
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-30T16:51:16.753129+02:00\""
---

Problem: a gate stopped by SIGTERM leaves its row processes running and its scratch root on disk. Measured 2026-09-30: `kill` of the `bin/hb --load test/run.f` root left 10+ row children spawning builds for over 10 s; each needed kill -9 by pid; the habu-native-suite-* scratch stayed. No exit hook runs on a signal (lib/fs-mutate.f CLEANUP-AT-EXIT runs on exit paths only; docs/db.md:313 records the limit). Acceptance: SIGTERM, SIGINT or SIGHUP to the gate root ends every descendant row process and removes the gate's scratch root within a bounded time, and the root's exit status shows the signal; the same holds for a row killed by the pool's deadline. An E2E test starts a small pool with a long row, signals the root, and asserts no descendant and no scratch remain. Files: test/gate-pool.f and the library the fix belongs in (lib/signal.f, lib/process*.f, lib/fs-mutate.f); docs/gate.md and docs/db.md for the rule. Verify: the new test; process-* and gate-pool suites. Depends: none. Ownership: signal handling of the gate pool and the cleanup registry. Claim: agent=kestrel workspace=.jj-ws/r4-signal.
