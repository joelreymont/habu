---
title: Guard engine stack extents at runtime
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:19:53.854959+02:00"
blocks:
  - habu-derive-fixed-data-853cb615
---

Static invariant: every native or recovery data-, user-return-, and loop-stack memory access is preceded by a bounds proof against the authoritative DATA-LAYOUT extent. Problem: J-TOR, J-RPOP, EMIT-P2-RS, J-FRAME, matching pops, and loop-frame paths can cross fixed capacities without a named guard, corrupting adjacent DATA cells before a diagnostic. Fix: derive stack bases/limits/capacities from DATA-LAYOUT, add fail-closed push/pop/frame guards at the shared native seams, and mirror the exact policy in bootstrap. Acceptance: zero-depth pops, capacity+1 pushes, 2>r/2r> boundary transfers, nested loop frames, address wrap, and mismatched native/recovery capacity reject before access with named errors; exact-last-slot operations pass; adjacent sentinels never change. Files: src/habu/layout.f, src/habu/habu2.f, bootstrap/cg/forth.fs, new test/engine-stack-bounds.f. Verify: focused mutation matrix, engine suite, bootstrap parity, clobber lint, typed-local diff, fixpoint, host/dot lints, full native gate. Dispatch only after DATA-layout and overlapping engine/bootstrap owners clear.
