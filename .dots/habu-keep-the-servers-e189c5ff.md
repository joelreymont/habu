---
title: "Keep the servers' run state out of images"
status: active
priority: 2
issue-type: task
created-at: "2026-11-25T17:31:36.394396+01:00"
---

Problem: the class of c79b723e remains in src/lib/net/http*.f (SLOT-REGION, JOB-*, PARK-*, WAKER, TAKEN-*, ROUTE-COUNT), src/lib/aio.f (WAKE-IN, EVENTFD-ARG, RING-*), src/lib/task.f TCB.EXIT-SLOT, mcp.f's held-call table, ws.f's timers and bearer.f's cells: a load that served leaves values in the program, a program that does it does not build (SLOT-REGION) or faults (ROUTE-COUNT), and two builds differ. Acceptance: such a load builds, its two builds are the same bytes, its run starts clean, and no image holds a secret a load fetched. Depends: c79b723e's lanes landing. Claim: agent=worker workspaces=.jj-ws.noindex/jerry-band5, jerry-band6.
