---
title: "A5: measure T0 against the JIT on arm64"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-29T03:23:36.283303+02:00"
closed-at: "2026-11-04T11:48:48.619282+02:00"
close-reason: "Census 2026-11-04, done: Measured (carry-the-jit notes: T0 run 12.61G vs JIT 12.76G); the go/no-go bar dropped by \"the T0 release\"; jerry-a5 not listed."
---

Workspace: jerry-a5 (to create on the integrated line after A3c, A3d, A3b2 and char). T0 driver from jerry-a4/.out/a4/a4-handoff.md (require t0-arm64.f t0.f, select aarch64, T0:INSTALL with a COUNTING-COMPILE through T0:DISPATCH!, bodies count printed through the chained exit hook as fs-mutate.f FS-EXIT:ARM), in the lead's .out. Measure per jerry-t0-l1/.out/a64-census.md sec. 4: zz load and run (jerry-aot/.out/bench zz-load.f, zz-run.f), JIT twin loading the same T0 sources without the install; tender once a load set resolves ../zen (run from a tree where ../zen exists). Bar: load <= 1.2x JIT instructions, run <= 1.05x, identical outputs, bodies count > 0 and no refusal. Go/no-go for A6.
