---
title: "V2 types: artifact typestate and evidence design"
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T10:25:02.710163+02:00"
blocks:
  - habu-checker-capability-typed-a480c423
---

Problem: MODEL-CAD-V2-PLAN.md:335-368 requires pass ordering and promotion evidence to be structural; current runtime report tags can be mixed across artifacts or bypassed by another call path. This is a bounded design/probe dot under 30 minutes. Fix: specify stage families for model/tensor-ir/region-ir/plan/kernel/candidate/artifact, artifact-indexed evidence families, promotion-policy products, and exact transition words; reuse existing SUMTYPE/PRODUCT/MATCH rather than inventing a parallel system. Acceptance: design contains untypeable wrong-order, incomplete-plan, wrong-artifact-evidence, and missing-gate fixtures and implementation subdots. Files: MODEL-CAD-V2-PLAN.md:335-368, maki/report.f, maki/cad.f:754-825, maki/store.f, docs/type-families.md. Verify: CHECK! candidate fixtures and current PROMOTE behavior census.
