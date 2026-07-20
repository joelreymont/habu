---
title: Synthetic-scenes end-to-end localization fixture
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T11:23:15.749968+02:00"
blocks:
  - habu-box-regression-losses-d84b0901
  - habu-patchify-op-with-a6dd25f1
  - habu-2d-positional-embedding-e9ef2217
  - habu-sigmoid-bce-and-c1604d5d
---

The flagship integration proof, deliberately requiring no file I/O: generate small synthetic scenes directly into image tensors (filled rectangles of random size, position, and intensity on a noise background, from the engine's deterministic RNG), train a tiny patchify + transformer + heads model to predict each rectangle's box and presence, and assert the losses fall below pinned thresholds with a fixed seed. This is the vision counterpart of the tiny-shakespeare fixture: it proves container, patchify, 2D positions, attention reuse, heads, losses, autograd, and AdamW compose end to end before any dataset or decoder exists. Keep it fast enough for the maki suite; a longer variant can live behind the perf-gated tier.
