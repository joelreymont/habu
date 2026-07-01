---
title: "PTX M9: bench + autotuner + precision"
status: open
priority: 2
issue-type: task
created-at: "2026-06-25T13:43:16.940801+02:00"
blocks:
  - habu-add-ptx-target-ba119d76
  - habu-make-ptx-device-c0eb12a3
  - habu-add-ptx-planner-30b93e8c
  - habu-fix-ptx-collective-997cfcce
---

File: PLAN.md:234 and PLAN.md:356. Gap: benchmark/autotune evidence must be
kernel-generic, target-aware, and shape-keyed; scalar SAXPY-style rows do not
drive GEMM/MMA/attention choices. Fix: use target capability records and the
generic PTX benchmark/profile API for candidate shape keys, warmups, samples,
variance rejection, bytes/FLOPs, selected roof, and selected plan reporting.
Verify: profile-test, bench-test, Orin candidate rows for elementwise,
GEMM/MMA, attention, and autotune-selected plans; no host timing masquerades as
GPU timing.
