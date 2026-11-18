---
title: Check the rejection builder across tasks
status: open
priority: 2
issue-type: task
created-at: "2026-11-18T07:27:44.208282+01:00"
---

Problem: src/core/rejection.f's builder state (TEXT, SAVED-P, SAVED-U, :17-47) is process-wide with no lock while the carry stamp is per-thread (src/habu/layout.f:766-774); two tasks writing records at once may interleave or take each other's saved record (inferred by the E-MAP design, not run). Acceptance: a reproducer shows whether two tasks' records interleave; if they do, the builder's state is per task and the reproducer becomes a test. Depends: none. Claim: unassigned.
