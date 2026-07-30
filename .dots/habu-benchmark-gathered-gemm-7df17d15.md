---
title: "Benchmark gathered GEMM: habu PTX vs Triton"
status: open
priority: 2
issue-type: task
created-at: "2026-07-30T23:12:42.306301+02:00"
---

Full context: decision measurement from the MLIR discussion 2026-07-30 (see joel.id/why-habu). Question: can habu's own PTX lowering hit competitive throughput on the post's flagship kernel (gathered GEMM, O[m n] = sum k A[IX[m] k] * B[n k]) versus Triton's MLIR-based lowering on the same DGX Spark hardware? Build both: the habu tile-op path through the existing ptx-stdlib machinery, and a Triton reference kernel; PROFILE both per the post's ladder (same shapes, same dtypes, warmed, repeated, variance reported); GOLDEN both against the host reference. Decision rule recorded in advance: if habu is within striking distance (define the threshold before measuring - suggest within 1.5x), the handmade checked path stays sole owner; if the gap is large and closing it means re-implementing per-generation tensor-core scheduling (wgmma/TMA class work), adopt Triton/MLIR as an OFFLINE digest-pinned artifact compiler behind the same file boundary discipline as ptxas - habu keeps surface, checking, autodiff, fusion decisions; the external tool does generation-specific scheduling; GOLDEN/PROFILE verify empirically. Device leg: DGX Spark (spark executor owns device runs).
