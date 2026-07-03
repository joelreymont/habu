---
title: Pool children die on parent death
status: open
priority: 2
issue-type: task
created-at: "2026-07-02T21:44:08.517699+02:00"
---

Deferred from habu-rca-tail-build-d6b0391d RCA. Secondary to the tail-build stall (root cause was BUILD-RUN inheriting stdin, fixed in lib/build.f). Forked test-pool workers (test/gate-pool.f, other-worker-owned) keep spinning as orphans after the harness SIGKILLs the parent suite. Fix: each pool worker installs a parent-death trigger and exits on parent death - Linux prctl(PR_SET_PDEATHSIG, SIGKILL); macOS has no PDEATHSIG, so poll getppid() != original in the pool loop or arm a kqueue NOTE_EXIT on the parent pid. Acceptance: after killing the top suite mid-run, no bin/hb pool children remain; add a regression that spawns a worker, kills the parent, and asserts the child is reaped.

## Progress (partial - staying open)

prctl/getppid/kqueue do NOT exist as primitives and their FPRIM registration
lives in src/habu/habu1.f (off-limits this session). Portable substitute
designed and PROVEN: a death pipe - the watched parent holds the write end; a
reaper in the child's process group closes every fd but the read end and blocks;
parent death EOFs the read end and the reaper kill(0,SIGKILL)s its group. Built
only from pipe/read/close/kill/setpgid (Linux + macOS).

Landed: test/gate-pool-orphan-test.f - a bounded regression (GPO-*) that forks a
watched parent P and a worker W (own group) whose reaper reaps W when P is
SIGKILLed, observed via an alive-pipe EOF within a hard deadline. Positive passes
(~0.45s); with the reaper disabled it FAILS at the deadline (proves it exercises
the mechanism, not a false pass). Kept test-local, not promoted to
lib/process-fork.f, to avoid a public stdlib API + manifest/docs churn while the
live wiring is unproven.

Deferred (see habu-reap-spawned-pool-fc4dc468): wiring the reaper into the LIVE
pool. Naive per-worker arming in GT-POOL-FORK-CHILD passes gate-pool-test and the
regression but HANGS the full gate in the stdlib/tail-process group (the forked
stdin-capture worker stops completing). Reverted to keep the gate non-hanging;
that dot carries the RCA lead, the forked+spawned wiring, and the resident-shard
gate wiring for the regression.
