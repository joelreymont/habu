---
title: 2D positional embedding for patch grids
status: open
priority: 1
issue-type: task
created-at: "2026-07-20T11:19:56.804505+02:00"
blocks:
  - habu-patchify-op-with-a6dd25f1
---

Extend the landed token+position embedding composition (the GPT wpe work) with a 2D grid variant: a learned embedding per (row, col) patch position, composed additively with patch projections. Reuse the embedding table machinery in maki/embedding.f; backward is the existing embedding scatter-add. Gradcheck plus a shape test proving a H-by-W grid and its flattened token order agree. Blocked by the patchify op only for the integration test that composes the two.
