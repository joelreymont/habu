---
title: Bilinear upsampling op with VJP
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T11:31:07.246838+02:00"
blocks:
  - habu-conv2d-op-via-3198a43a
---

Bilinear interpolation over feature maps (integer scale factors first, arbitrary sizes second), needed for the feature pyramid's top-down path and generally for resizing inside a model. Backward distributes each output gradient to its four source corners by the same interpolation weights - the exact transpose. CAD-registered, gradchecked including edge-pixel and scale-1 identity cases, torch-reference fixtures with align-corners semantics decided once and documented.
