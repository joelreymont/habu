---
title: "Checker capability (c): register accumulator tile type"
status: open
priority: 2
issue-type: task
created-at: "2026-06-27T18:10:55.991897+02:00"
---

Sub-dot of habu-checker-capability-typed. Add a register-resident accumulator tile type threaded across TILE-LOOP (the C micro-tile in GEMM, the running max/sum in attention). Decide whether the existing tile<t,b,m> suffices as the accumulator or a distinct acc<t,b,m> is needed to forbid accidental global store of an unfinished accumulator. Files: lib/ptx/tile-acc.f (or extend tile-loop.f) + tests + TRUSTED.md + ptx-stdlib wiring. Dep: parent; composes with (a) TILE-LOOP and (b) shared tiles.
