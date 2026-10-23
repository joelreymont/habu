---
title: Give gate-pool-orphan a readiness signal
status: open
priority: 3
issue-type: task
created-at: "2026-10-23T17:33:29.588537+03:00"
---

Problem: gate-pool-orphan waits on ~1.2 s of sleeps because the reaper joins the leaf's process group from its own side, so a reported pid does not mean it is ready to be killed (g6 test audit lane).
Acceptance: the test waits on a real readiness signal from src/lib/process-fork.f instead of sleeps; the orphan-reaping assertions are unchanged.
Verification: the suite passes repeatedly under load with lower wall time.
