---
title: Spatial max-pool and avg-pool with VJPs
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T11:30:37.411656+02:00"
blocks:
  - habu-conv2d-op-via-3198a43a
---

Spatial downsampling over feature maps: max-pool (argmax recorded forward, gradient routed to the argmax in backward - ties broken deterministically and documented) and average-pool (uniform broadcast backward), both with kernel/stride/padding parameters. CAD-registered, gradchecked including tie and boundary-padding cases, torch-reference fixtures. Distinct from the ViT epic's token mean-pool, which reduces over the token axis; these reduce over the spatial grid.
