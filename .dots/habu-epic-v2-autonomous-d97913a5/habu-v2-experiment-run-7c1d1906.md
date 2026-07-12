---
title: V2 experiment run identity
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:25:27.690944+02:00"
blocks:
  - habu-v2-canonical-artifact-ee5121b4
---

Implement MODEL-CAD-V2-PLAN.md:1672-1689 immutable dataset/split/preprocess/seed/model/optimizer/numeric/target/compiler/environment run keys and typed metric populations. Acceptance: every semantic mutation changes the run id, equal keys resume the same lineage, held-out test metrics cannot be consumed as training objectives, missing license/authority rejects, and deterministic next-batch identity is pinned. Files: maki/experiment/run.f plus focused tests.
