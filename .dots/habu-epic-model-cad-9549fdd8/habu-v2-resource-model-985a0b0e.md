---
title: V2 resource model calibration
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:25.121508+02:00"
blocks:
  - habu-cad-6-tune-a573112c
---

Problem: MODEL-CAD-V2-PLAN.md:1397-1417 requires validated occupancy/register/shared/launch estimates; current schedule selection lacks calibrated prediction error. Fix: add exact resource fields to Plan IR and record predicted versus ptxas/device values for one GEMM family. Acceptance: over-smem and invalid occupancy candidates reject; prediction error is reported; stale target/toolchain invalidates calibration. Files: maki/cost.f, maki/mem-plan.f, maki/schedule.f, tools/ptx/bench.f. Verify: estimator fixtures, ptxas metadata comparison, same-session profile.
