---
title: Annotated image dataset container and loader
status: open
priority: 2
issue-type: task
created-at: "2026-07-20T11:23:15.757562+02:00"
blocks:
  - habu-image-tensor-container-0aa984c4
---

A dataset layer for real training runs: a directory of raw image tensors (the container format from the image-container dot, written by an offline export step) plus a simple annotation table (boxes and labels per image) parsed by a checked reader. Batched loading follows maki/batch-loader.f patterns; fixtures live under private per-run temp roots per the corpus-isolation lesson (unique TMPDIR-MKDIR root, symlink rejection, exception-safe cleanup). Defines the on-disk layout once, documented, with a versioned header so later decoders feed the same container.
