---
title: Watch a test-pool reaper that dies at start
status: open
priority: 2
issue-type: task
created-at: "2026-11-12T13:57:42.745496+02:00"
---

Problem: test/gate-pool.f:403-408 GT-POOL-KILL-REAPER discards the reaper's ending (PROCESS:WAIT drop), so a reaper that died at start (exit 78, the fork alias failure of habu-a-forked-child-750083bd) is silent and its slot runs with nothing watching it. Acceptance: a reaper that ends other than by the pool's own SIGKILL is reported on the pool's output with the slot's label and its ending, and the slot's test counts as failed. Depends: none. Claim: unassigned.
