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

Revalidated audit M1/M2 on 2026-09-14 at source 220930e479bdff0f21819944ea6aefbfe380e798 and optimized native engine SHA-256 2a49a29c9804f00292652d45f0a32aa27e6f224034585501e890bf9d2acbc14c. EM-RUNTIME-STACK (habu2.f:4558) reserves 16384 bytes below process SP; G-PUSH (rt.f:29) stores then increments without an upper bound, and the LMAIN guard (habu2.f:6527) checks only the floor. A read-only live geometry probe confirms argv minus S0 is 16392 bytes: 2048 cells plus argc. RSTK-PUSH/POP (habu1.f:1562) remain unguarded, with RSTK-OFF $2800 plus 256 cells ending at LOCNAMES $3000. Native disassembly of public 2>r confirms unchecked stores/depth increments; the safe 1 2 2>r 2r> pair returns 1 2.

The original compiled >r reproducer is not evidence for tier 1: src/compiler/native/elaborate.f models ordinary return transfers as compile-time value vectors with checked vector capacity, without accessing the engine RSTK region. Fix the still-live primitive and actual stack-access paths; do not restore old host/compiler machinery to reproduce the old audit. No overflowing data/return-stack operation was executed during this revalidation. The existing runtime guard task owns both findings; peak-certificate work remains a separate child of the stack coordinator.
