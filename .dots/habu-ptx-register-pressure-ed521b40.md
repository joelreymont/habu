---
title: PTX register-pressure allocator (virtual, occupancy-driven)
status: open
priority: 1
issue-type: task
created-at: "2026-07-18T15:10:25.656577+02:00"
blocks:
  - habu-codegen-verdict-roofline-4d6bf436
---

Joel-confirmed (2026-07-18): we need our own allocator at the VIRTUAL level; physical assignment stays ptxas's. Component of the typed-IR layer (habu-ptx-opt-layer-325b9507). Scope: live-range model over emitted virtual registers; occupancy targeting from the process row (regs/thread x smem/block vs SM limits, sm_87 + sm_121a rows); decisions = tile/accumulator blocking, remat-vs-keep-live, unroll/pipeline depth caps, spill-to-SMEM policy (never local memory); replaces the hand-blocking baked into per-family emitters (lower-mm 48/56-reg choices become allocator outputs). Evidence baseline: habu-codegen-verdict-roofline-4d6bf436 SASS audit counts current reg pressure + LDL/STL spills to quantify what hand-shaping achieved. Generalizes register-blocked GEMM to arbitrary SPEC: dataflow — load-bearing for the Triton reimpl.
