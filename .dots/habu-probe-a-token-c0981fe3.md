---
title: Probe a token rollback under a live task
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T10:08:57.787102+01:00"
---

Problem: src/habu/interpreter.f:3069 FRAME-ROLLBACK-TOKEN and src/habu/habu2.f:2167 (evaluate's recovery) lower CP to TOKCP whether or not a task is live; the GitHub x86 branch (98fe1027) kept CP while TASKS-LIVE-CELL was nonzero. If a failing token can publish code a task then runs (a quotation, a DOES> body), the truncation lets later code overwrite it. Acceptance: a test shows a live task's code survives a token that throws after publishing it, or the reproducer shows the overwrite and both rollbacks keep CP while a task is live. Depends: none. Claim: unassigned.
