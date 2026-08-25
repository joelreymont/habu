---
title: Sliced high-resolution inference
status: open
priority: 3
issue-type: task
created-at: "2026-07-20T11:31:07.273415+02:00"
blocks:
  - habu-synthetic-few-pixel-9096b713
---

Inference-time capability for frames too large to process whole: tile the input with overlap, run detection per tile, merge boxes across tile seams with non-maximum suppression aware of the overlap regions. Pure composition on the landed model; fixtures with targets straddling tile boundaries. Deferred until a real workload needs frames beyond the training resolution.
