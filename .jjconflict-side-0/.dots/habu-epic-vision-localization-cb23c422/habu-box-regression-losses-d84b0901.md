---
title: "Box regression losses: L1 and generalized IoU"
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T11:19:56.821683+02:00"
---

Localization needs box losses: elementwise L1 on (cx, cy, w, h) boxes and generalized IoU with its exact subgradient, including the degenerate cases (zero-area boxes, disjoint boxes, containment) which each get a pinned fixture. Torch-reference values committed as data. Pure host-side tensor math first; CAD registry registration and GPU lowering are the separate lowering dot. Design note: pick the box parameterization once here and document it - every later head and dataset loader inherits it.
