---
title: "PTX kernels: RMSNorm + RoPE checked"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T00:37:53.008350+02:00"
---

Driving workload demand (LocateAnything port: kernel-order, coalescing, and fusion analyses; docs/model-cad.md Driving workload). RMSNorm: row-reduction family kernel (one row per block, coalesced hidden-dim load, reduce + rsqrt + scale one kernel) alongside existing SOFTMAX-ROWS machinery in lib/ptx/collective.f; golden vs CPU reference; AD: VJP entry + gradcheck. RoPE: pointwise pair-rotation kernel over [B,heads,S,head_dim], adjacent lanes on adjacent head_dim pairs; fusion candidate with QKV layout transform noted for cad-2 planner. Both checked KERNEL: definitions, sm_87 golden on Orin, profile rows. Related: habu-checker-capability-typed-e0c76a02, habu-re-express-fused.
