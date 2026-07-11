---
title: V2 MMA schedule integration
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T12:16:00.318252+02:00"
---

Problem: MODEL-CAD-V2-PLAN.md:1397-1417 requires tensor-core compute parity; habu-tensor-core-mma-11f23a94 owns raw MMA codegen but no Model CAD schedule-to-plan integration. Fix: add one sm_87 TF32 MMA schedule family whose Plan IR explicitly carries MMA shape, accumulator domain, BK, warps, stages, and layout; no lowering defaults. Acceptance: legal 128x128x32 plan lowers; unsupported dtype/shape/layout rejects before PTX; device golden and same-session Triton row recorded. Files: maki/schedule.f, maki/lower-mm.f, maki/plan-ir.f, lib/ptx/cg-matmul.f. Verify: schedule/plan/lower tests plus Orin GEMM golden and profile. Depends: habu-tensor-core-mma-11f23a94.
