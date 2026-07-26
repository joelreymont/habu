---
title: V2 resource model calibration
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:25.121508+02:00"
blocks:
  - habu-cad-6-tune-a573112c
  - habu-attest-proprietary-ptxas-6ce9fda2
  - habu-measure-gpu-compiler-086c1a28
  - habu-validate-gpu-reduction-ff746ff8
---

Problem: MODEL-CAD-V2-PLAN.md:1397-1417 requires validated occupancy/register/shared/launch estimates; current schedule selection lacks calibrated prediction error. Fix: add exact resource fields to Plan IR and record predicted versus ptxas/device values for one GEMM family. Acceptance: over-smem and invalid occupancy candidates reject; prediction error is reported; stale target/toolchain invalidates calibration. Files: maki/cost.f, maki/mem-plan.f, maki/schedule.f, tools/ptx/bench.f. Verify: estimator fixtures, ptxas metadata comparison, same-session profile.

Compiler-IR reconciliation: this dot is the target resource-fact and calibration
owner consumed by GPU-GIR validators and artifact evidence in Waves D-E. It does
not own KIR/GIR schemas, schedule search, PTXIR2, or promotion authority.
