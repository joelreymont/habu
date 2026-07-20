---
title: "EPIC: convolutional backbone and multi-scale for small targets"
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T11:30:18.398884+02:00"
---

Second phase of the vision program, sequenced after the ViT epic habu-epic-vision-localization-cb23c422 proves end to end: detecting few-pixel targets in high-resolution frames, where a plain patchify transformer is weakest (a 16x16 tile can swallow the whole object, and global attention has no locality prior). Plan: convolutional feature extraction and a multi-scale feature pyramid feeding the same detection heads and losses the ViT epic lands. Convolution arrives via im2col - overlapping patchify feeding the existing GEMM - so the ViT epic's gather machinery and matmul kernels are the on-ramp; only the final direct GPU convolution kernel is new kernel engineering. The likely production shape is hybrid (convolutional early layers for locality and resolution, transformer body for context), which keeps every ViT-epic investment in play.
