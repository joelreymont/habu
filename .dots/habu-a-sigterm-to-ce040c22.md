---
title: A SIGTERM to a test-pool reaper ends the whole pool
status: active
priority: 2
issue-type: task
created-at: "2026-11-12T14:30:01.844985+02:00"
---

Problem: a SIGTERM sent to one slot's reaper (src/lib/process-fork.f SPAWN-REAPER :243-253, armed by test/gate-pool.f GT-POOL-SPAWN-REAPER!) ends the whole test pool (`test pool: signal 15, ending 1 live rows`, rc 143) instead of that slot alone. Cause not traced; the inference is that the forked reaper keeps the pool's signal handling. Acceptance: a SIGTERM to one reaper ends that reaper only, and the pool reports it and fails that slot alone; the fix sits at the traced cause. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-sigreap.
