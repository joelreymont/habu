---
title: Catch a refused reaper arm in pool workers
status: open
priority: 3
issue-type: task
created-at: "2026-10-02T12:39:04.024626+02:00"
---

Problem (review 304): test/gate-pool.f:818-830 GT-POOL-FORK-CHILD calls GT-POOL-ARM-REAPER (PROC-FORK:FORK-REAPER, :809) before its 'q catch', and test/gate-pool-orphan-test.f:64 GPO-WORKER has the same shape, so a throw from FORK-REAPER in a forked worker (E-PROC-SPAWN on a refused first fork since e68ae79c; E-PROC-WAIT before it) unwinds into the worker's copy of the pool parent's stack instead of ending the worker. Acceptance: the worker catches a failed reaper arm and exits through GT-POOL-FORK-EXIT with a status the pool parent reports as that failure; shown by forcing the refusal in a worker (RLIMIT_NPROC as lib/process-fork-test.f does) through the real pool path, seen failing first. Files: test/gate-pool.f, test/gate-pool-orphan-test.f.
