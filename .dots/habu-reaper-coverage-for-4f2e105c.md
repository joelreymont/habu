---
title: Reaper coverage for worker-internal capture spawns
status: open
priority: 3
issue-type: task
created-at: "2026-07-06T15:28:35.325148+02:00"
---

Residual from habu-pool-children-die-6e57e753 acceptance run (2026-07-06, head 8f096ded). Pool-slot children are parent-death covered (fork workers via GT-POOL-ARM-REAPER worker-alive+death-pipe reaper; pool spawns via GT-POOL-ARM-SPAWN-REAPER co-located group reaper). NOT covered: children a fork worker spawns ITSELF via PROC-RUN-CAPTURE / PROC-RUN-ARGV-CAPTURE etc. (each spawn is its own process-group leader per the spawn primitive, so the worker-group SIGKILL misses it, and no co-located reaper is armed). Observed: after SIGKILLing the top suite mid-gate, two such leaves survived ~3-8s (habu-test-runner ok.f and tools/aot-call-report.f) then self-terminated via pipe EOF/completion - bounded in practice because capture children write to pipes whose readers died, but a long-running leaf that writes nothing (pure compute, e.g. an AOT report over a large image) could linger for its full runtime. Fix: plumb the pool death-pipe read end into the worker's PROC-RUN-* spawn path and arm PROC-SPAWN-REAPER (lib/process-fork.f:138) for capture spawns made inside a pool worker context - the worker already holds GT-POOL-DEATH-RD before GT-POOL-ARM-REAPER closes it (test/gate-pool.f:577-583), so the capture path needs a worker-context cell carrying the fd (or re-arming from the reaper side). Acceptance: repeat the top-suite SIGKILL experiment while a deliberately long quiet leaf (sleep-loop hb child under PROC-RUN-CAPTURE) is running; the leaf must die within the reaper deadline, not at its own completion; plus a bounded regression beside test/gate-pool-orphan-test.f.
