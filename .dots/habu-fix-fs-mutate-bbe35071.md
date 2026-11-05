---
title: Fix fs-mutate REMOVE-TREE socket flake
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T17:33:29.557618+03:00"
---

Problem: fs-mutate's 'REMOVE-TREE unlinks a bound Unix socket and a nested FIFO' failed once on macOS arm64 with -2105 ('staged replacement cleanup threw -2105', 7.39 s) and passed on two reruns (g5 test audit lane, host load ~20).
Acceptance: the root cause is found and fixed at its layer; the case passes 50 consecutive runs under load.
Verification: repeated bin/hb --load runs of the fs-mutate suite.

Cause found 2026-11-05 (carl): the socket fixture's path grows with TMPDIR (test/gate-pool.f:494 hb-sock under the suite's TMPDIR). In .jj-ws.noindex/carl-s6-l2 (TMPDIR=<ws>/.out/tmp) socat warns "unix socket address 105 characters long, truncating to 104" and the case fails with the -2105 cleanup throw; the same engine and tree pass with TMPDIR=/tmp/cfsm, and carl-base (one character shorter) passes. Acceptance becomes: the socket sits at a path whose length does not depend on TMPDIR, as habu-keep-pg-kill-391bfc0d asks for pg; a long TMPDIR passes.
