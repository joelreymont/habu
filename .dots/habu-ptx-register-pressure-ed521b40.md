---
title: PTX register-pressure allocator (virtual, occupancy-driven)
status: open
priority: 3
issue-type: task
created-at: "2026-07-18T15:10:25.656577+02:00"
---

Joel-confirmed (2026-07-18): we need our own allocator at the VIRTUAL level; physical assignment stays ptxas's. Component of the typed-IR layer (habu-ptx-opt-layer-325b9507). Scope: live-range model over emitted virtual registers; occupancy targeting from the process row (regs/thread x smem/block vs SM limits, sm_87 + sm_121a rows); decisions = tile/accumulator blocking, remat-vs-keep-live, unroll/pipeline depth caps, spill-to-SMEM policy (never local memory); replaces the hand-blocking baked into per-family emitters (lower-mm 48/56-reg choices become allocator outputs). Evidence baseline: habu-codegen-verdict-roofline-4d6bf436 SASS audit counts current reg pressure + LDL/STL spills to quantify what hand-shaping achieved. Generalizes register-blocked GEMM to arbitrary SPEC: dataflow — load-bearing for the Triton reimpl.

VERDICT UPDATE 2026-07-18 (habu-codegen-verdict, closed): NOT on the critical path — SASS audit on GB10 shows zero spills on every kernel (56/40 regs of 255, no LDL/STL), hand-blocking is currently sufficient. Re-prioritize when the SPEC:-driven opt layer (habu-ptx-opt-layer-325b9507) needs allocator-driven blocking for arbitrary dataflow; until then this is design-ahead work, p3.
