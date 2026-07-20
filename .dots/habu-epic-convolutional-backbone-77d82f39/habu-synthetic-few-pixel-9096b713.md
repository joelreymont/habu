---
title: Synthetic few-pixel-target fixture
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T11:31:07.261084+02:00"
blocks:
  - habu-groupnorm-with-vjp-b468f410
  - habu-feature-pyramid-composition-294d5883
  - habu-spatial-max-pool-ff2fba18
---

The small-target counterpart of the ViT epic's synthetic-scenes flagship, still requiring no file I/O: generate high-resolution frames with targets of only a few pixels (2 to 6 px squares at random positions, low contrast against structured noise), train the convolutional pyramid model with the shared detection heads and losses, and pin a recall threshold at a fixed seed that the plain ViT model demonstrably fails (run both, commit both numbers - the gap is the epic's justification made measurable). Keep the suite-tier variant fast; the full-resolution variant lives behind the perf-gated tier.
