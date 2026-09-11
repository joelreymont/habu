---
title: Port known-good GPU matrix
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:03:22.160381+02:00"
blocks:
  - habu-validate-gpu-matrix-cd045456
---

Full context: port one measured known-good GEMM/MMA configuration through KIR, GIR, PTXIR2, ptxas, launch, and promotion evidence before broadening search. Acceptance: current exact/device correctness passes and pinned resource/performance baseline is met or exceeded; no hand PTX register ranges remain in semantic lowering.
