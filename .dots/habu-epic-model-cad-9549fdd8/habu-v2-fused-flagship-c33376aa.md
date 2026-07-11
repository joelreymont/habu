---
title: V2 fused flagship matrix
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:25.540257+02:00"
blocks:
  - habu-small-model-end-f7cc1b39
---

Problem: MODEL-CAD-V2-PLAN.md:1438-1452 and 1530-1545 require an end-to-end region win, not isolated SAXPY. Fix: build the first linear+bias+GELU+residual region comparison across unfused Model CAD, fused Model CAD, hand-fused Triton, torch.compile/Inductor, and applicable vendor-library composition. Acceptance: equal licensed accuracy, strictly fewer bytes or launches than unfused, exact same-session protocol, measured workload-local latency win or an explicit red result. Files: maki/model-ir.f, maki/fusion-plan.f, maki/eval-device.f, docs/eval-triton.md. Verify: host golden, Orin device golden, profile/evidence row.
