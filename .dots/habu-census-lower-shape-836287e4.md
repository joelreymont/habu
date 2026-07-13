---
title: Census lower shape products
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:14:22.923400+02:00"
blocks:
  - habu-implement-cad-num-cb413b2a
---

Full context: B5 must not duplicate Maki DIM*, SHAPE-ELEMS, or TENSOR-BYTES arithmetic. Perform a read-only census of maki/tensor.f, maki/tensor-value.f, maki/cad.f, maki/executor.f, maki/golden-artifact.f, maki/gradcheck.f, maki/lower-ew.f, maki/lower-launch.f, maki/lower-mm.f, maki/lower-move.f, maki/lower-red.f, maki/move-view.f, maki/plan-ops.f, maki/saved.f, and maki/traffic.f. Classify every raw product as already owned by one of those APIs or name an exact residual file/word and create a separate owner dot before editing. Acceptance: committed census in MODEL-CAD-V2-PLAN.md with zero/overflow semantics and no Maki source change; tensor/tensor-value tests green.
