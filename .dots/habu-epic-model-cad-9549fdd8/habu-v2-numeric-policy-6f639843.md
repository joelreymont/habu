---
title: V2 numeric policy schema
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:25.421270+02:00"
blocks:
  - habu-v2-research-approximation-c10e7cc6
---

Problem: MODEL-CAD-V2-PLAN.md:1471-1482 requires precision in every plan/artifact key; current flags can silently compare FP32 FMA and TF32 tensor-core results. Fix: implement exact/ULP/relative/empirical policy values and attach them to rewrite, schedule, golden, and promotion records. Acceptance: approximate evidence cannot satisfy exact policy; deterministic composition is tested; changing policy invalidates plan/artifact/tuning keys. Files: maki/precision.f, maki/golden.f, maki/schedule.f, maki/promotion.f. Verify: TF32/GELU/recompute positive and negative fixtures.
