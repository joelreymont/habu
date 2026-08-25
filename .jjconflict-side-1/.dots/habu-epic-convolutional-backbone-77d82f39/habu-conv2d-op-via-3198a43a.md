---
title: Conv2d op via im2col with exact VJPs
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T11:30:37.404044+02:00"
blocks:
  - habu-synthetic-scenes-end-6520cca1
---

The core convolution op, correctness-first: express conv2d as im2col (overlapping patchify - a strided gather generalizing the ViT epic's patchify) feeding the existing matmul, with stride, padding, and dilation parameters. Two exact adjoints: the input gradient (col2im scatter-add, the transpose of the gather) and the weight gradient (a second matmul over the unfolded columns). Register in the CAD op registry like the other ops (same SERIALIZE caution on registry file ownership), gradcheck both adjoints at several shapes including stride>1, asymmetric padding, dilation>1, and 1x1 kernels, plus torch-reference fixtures committed as data. Perf is explicitly out of scope here - the direct GPU kernel is its own later dot; this dot owns semantics.
