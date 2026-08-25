---
title: GroupNorm with VJP and normalization decision
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T11:30:37.419493+02:00"
blocks:
  - habu-conv2d-op-via-3198a43a
---

Convolutional stacks need a normalization layer that works at the small batch sizes detection training actually uses. Decide once here between BatchNorm and GroupNorm and document the choice in the dot closure and docs (expected outcome: GroupNorm - batch-size independent, no train/eval statistics split, the standard choice for modern detection; record why BatchNorm was rejected, including its running-statistics state that complicates exact checkpoint resume). Implement the chosen norm with exact VJP, gradcheck across group counts including groups=1 (LayerNorm-like) and groups=channels (InstanceNorm-like), torch-reference fixtures.
