---
title: Feature pyramid composition
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T11:31:07.254395+02:00"
blocks:
  - habu-bilinear-upsampling-op-632dc234
  - habu-conv2d-op-via-3198a43a
---

Multi-scale features - the standard remedy for few-pixel targets: lateral 1x1 convolutions on backbone stages plus a top-down pathway of upsample-and-add, producing pyramid levels that the detection heads run on per level. Composed from the landed conv2d and upsampling ops in the CAD plan (this dot is model composition plus tests, not new primitives). Fixture proves shapes and gradients flow through all levels, and that a target visible only at the finest level produces loss signal there.
