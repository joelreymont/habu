---
title: Enable EW prologue fusion into matmul regions
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T20:12:23.595106+02:00"
---

Leg (b) of habu-automatic-aggressive-fusion-828cdeb3, split out at its 2026-07-15 closure: EW->MATMUL fusion is LEGAL (FP-BASE-FUSE?) but the backend cannot emit it - the documented FP-CAP gap 'lower-mm cannot pre-transform A/B (E-LMM-PROLOGUE)' in maki/fusion-plan.f's capability table. Fix when the lower-mm fence releases: teach the matmul emitter to apply elementwise prologue ops to its A/B tile loads in registers (scale-before-matmul etc.), then flip the FP-CAP EW->MATMUL bit + add the planner regression (the capability-table comment documents this exact flip protocol) + a device golden for a scale->matmul model. Related but distinct from habu-widen-lmm-epi-ff428ed4 (epilogue side). FENCED: maki/lower-mm.f is sol's region territory (habu-v2-r3-type-144b5fa2) + makipools remainder - coordinate at dispatch. Files: maki/lower-mm.f, maki/fusion-plan.f (one bit + regression), device test. Verify: lower-mm tests, maki/test.f, on-device golden. Ownership: maki matmul lowering.
