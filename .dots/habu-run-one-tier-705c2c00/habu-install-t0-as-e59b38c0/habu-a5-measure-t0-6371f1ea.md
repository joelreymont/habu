---
title: "A5: measure T0 against the JIT on arm64"
status: active
priority: 2
issue-type: task
created-at: "2026-10-29T03:23:36.283303+02:00"
blocks:
  - habu-compile-char-in-d98cb44f
---

Workspace: jerry-a5 (to create on the integrated line after A3c, A3d, A3b2 and char). T0 driver from jerry-a4/.out/a4/a4-handoff.md (require t0-arm64.f t0.f, select aarch64, T0:INSTALL with a COUNTING-COMPILE through T0:DISPATCH!, bodies count printed through the chained exit hook as fs-mutate.f FS-EXIT:ARM), in the lead's .out. Measure per jerry-t0-l1/.out/a64-census.md sec. 4: zz load and run (jerry-aot/.out/bench zz-load.f, zz-run.f), JIT twin loading the same T0 sources without the install; tender once a load set resolves ../zen (run from a tree where ../zen exists). Bar: load <= 1.2x JIT instructions, run <= 1.05x, identical outputs, bodies count > 0 and no refusal. Go/no-go for A6.
