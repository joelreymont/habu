---
title: Make gate-runner-entry a faithful oracle
status: open
priority: 2
issue-type: task
created-at: "2026-08-05T12:23:19.290842+02:00"
---

test/gate-runner-entry.f -- <group> produces ~17 reds for tail-pure that the full test/run.f does not (E-UNDERFLOW: allot, duplicate definition: ARM64-W32) — the standalone runner does not reproduce the pool's load environment, so it cannot be used to bisect a pool red. Either make the entry runner load the same base the pool workers inherit, or document exactly what it can and cannot answer; a debugging oracle that lies costs an investigation each time (the pool-crash lane lost a cycle to it).
