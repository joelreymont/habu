---
title: V2 compiled training parity
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:25.599571+02:00"
blocks:
  - habu-autograd-end-to-ee4d918b
---

Problem: MODEL-CAD-V2-PLAN.md:1484-1500 requires PyTorch-level compiled training evidence. Fix: route one temporal/MLP training step through generated backward, save/recompute plan, fused optimizer, mixed-precision policy, and the common artifact pipeline. Acceptance: per-step gradients and convergence match independent reference tolerance; checkpoint/resume is deterministic; profile separates forward/backward/optimizer and records fusion. Files: maki/autograd.f, maki/train.f, maki/optim.f, maki/checkpoint.f. Verify: seeded host parity, Orin training golden, convergence/profile rows.
