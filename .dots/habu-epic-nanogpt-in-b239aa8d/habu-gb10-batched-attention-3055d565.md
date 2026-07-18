---
title: GB10 batched-attention tiling plan node
status: open
priority: 2
issue-type: task
created-at: "2026-07-18T17:36:22.599569+02:00"
blocks:
  - habu-extent-roles-b-df9d232f
---

Consume the #B/#T/#H extents in the movement/schedule planner to lower batched attention on GB10: batch/head as outermost grid extent, each (b,h) a T x hd TMA-movable tile, flash-tiled when T x T exceeds the 99KB SMEM budget (tma-gather.md:90-92; compute-campaign.md:39-45). Records lowering + evidence like the MOVE plan node. North-star lane, off the numeric-golden critical path. Full contract: docs/batch-sequence-design.md section 5 BTC-6.
