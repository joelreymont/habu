---
title: Direct GPU convolution kernel
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T11:31:07.267244+02:00"
blocks:
  - habu-gpu-lowering-for-052ebd60
  - habu-conv2d-op-via-3198a43a
---

The perf successor to the im2col path: an implicit-GEMM convolution kernel in PTX that never materializes the unfolded column matrix, integrated with the autotune machinery and benched against the im2col-plus-GEMM baseline with parity fixtures at detection-realistic shapes (large spatial extent, small channel counts early; the reverse late). Only claim after the im2col path is the proven bottleneck on a real training run - measure first; the whole point of the im2col route is that this dot may wait a long time.
