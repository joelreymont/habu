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

UPDATE 2026-07-17 (wave2 landing cb8fa57e): new measured best is
MMM-WIDE-M4-S1 (MMA-MFRAGS=4 BK=32 pad=8 stages=1 STATIC 49152B ldmatrix,
256x64 block): 2707.3 GFLOP/s = 1.43x Triton at 2048^3/918MHz; MFRAGS=4
stages=2 dynamic is occupancy-bound and slower (2394.1). Static smem means
no dynamic-launch coupling for this config, but block-M grids now need
M multiple of 256 (MFRAGS=4) - the autotuner must key MFRAGS by shape
(512^3 favors it at 2032.7; ragged/small M needs MFRAGS<=2 or 1).

UPDATE 2026-07-17 (wave3 landing 58faceba): new measured best is
MMM-WIDE-B-M4-S1 (MMA-BLDM=1 bpad=4 MFRAGS=4 BK=32 pad=8 stages=1
single-buffer DYNAMIC 50176B): 3026.6 GFLOP/s = 1.60x Triton at
2048^3/918MHz. New axes for the autotuner: MMA-BLDM (0/1) and MMA-BPAD
(multiples of 4 only - emitter fail-closes otherwise); shape keying
unchanged (M multiple of 256 at MFRAGS=4). 512^3 favors it at 1802.6,
1024^3 at 2746.0.

FOLDED 2026-07-18 (from habu-v2-checked-async-8d460576 closure, casync
lane): two follow-ons belong to this autotuner dot:
1. Wire CPLEGAL:REQUIRE (maki/cp-async-legal.f) into per-target candidate
   legality once stage selection flows to lowering — REQUIRE
   ( bufb stages target-id -- ) consults the real target descriptor
   (TARGET:CAP-ASYNC / TARGET:CAP-BARRIER / SHARED@), throws
   E-CP-ASYNC-TGT (-5082) fail-closed. Today depth is a fixed global
   config; wiring is due exactly when the autotuner starts choosing
   stages/buffers per shape+target (MMA-BLDM/MMA-BPAD/MFRAGS axes above).
2. Add a focused regression for the existing lib-local MMA-CHECK-SMEM /
   E-MMA-SMEM guard (currently untested); cp-async-legal-test.f is the
   fixture pattern to copy (raw-descriptor pos/neg + interned-id path).
