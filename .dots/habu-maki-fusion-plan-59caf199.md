---
title: "Maki: fusion-plan must materialize a movement model-output"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T21:50:16.701979+02:00"
---

SLICE 4 found a fusion-plan.f gap: FP-MAT-FLAG (maki/fusion-plan.f) marks a movement node materialized only when NODE-MAT-VD? (materialize/gathered verdict) OR FP-REGION-OUT? (crosses a region boundary). A free/staged movement that IS the model output (no consumer, not cross-region) stays mat=0, so its region has ZERO materialized outputs and the copy kernel fails closed E-LMV-NOOUT (maki/lower-move.f LMV-FIND-NODE). Empirically: MODEL: T ( x:4x8 -- y ) TRANSPOSE ; and standalone SLICE(free) both leave mat=0. Workaround in tests: force -1 <node> MIR-MAT! or use a multi-use fan-out (TRANSPOSE >V H H RESIDUAL-ADD) so FP-REGION-OUT? materializes it (used by maki/lower-mv-device-test.f). Fix (fusion-plan.f, not slice-4 surface): FP-MAT-FLAG should also set mat=1 for a movement node with FP-REF-USES 0 (a model output), exactly like the compute-node model-output branch below it. Then standalone TRANSPOSE/SLICE/CONCAT/GATHER lower via LMV-RUN without forcing.
