---
title: Feed MMA config axes to shape-keyed autotuner
status: open
priority: 2
issue-type: task
created-at: "2026-07-15T22:11:50.772296+02:00"
---

Residual from habu-mma-larger-bk-1ae1c6b2 (2026-07-15): the MMA emitter is now cleanly parameterized (MMA-BK / MMA-PAD / MMA-STAGES / MMA-DYNSMEM / fragment LMODE with fail-closed E-MMA-SMEM legality) but the cad-6 shape-keyed autotuner does not search these axes - wiring requires the FENCED maki/lower-mm.f (LMM-MMA? dispatch) and maki/schedule.f (gemm-tf32-v1 family key). Fix when the fence releases: add the (BK, pad/swizzle, stages, dynsmem, lmode) axes to the gemm schedule family, key results by shape + config, keep mma-gemm-check element-exact for every searched point, and let promotion pick the measured best per shape. Depends on: ship-swizzled-default dot (or lands with it). Files: maki/lower-mm.f, maki/schedule.f, sched-key fixtures. Verify: schedule/sched-key tests, maki/test.f, on-device sweep. Ownership: maki GEMM autotuning.

UPDATE 2026-07-17 (bfeed landing 7c7e7102): the measured best is now
MMM-WIDE-M2 (MMA-MFRAGS=2 BK=32 pad=8 stages=2 dyn ldmatrix, 128x64 block,
2133.9 GFLOP/s = 1.13x Triton at 2048^3/918MHz; stages=1 static variant
2084.1). Add MMA-MFRAGS to the searched axes; note the block-M-aware launch
(gridY = M/(64*MFRAGS), shapes multiple of 128) when wiring lower-mm dispatch.
