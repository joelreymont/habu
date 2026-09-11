---
title: Schedule GPU matrix pipelines
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:03:22.131440+02:00"
blocks:
  - habu-model-gpu-contractions-85d74a87
---

Full context: lower contractions to GPU-GIR with shared staging, async-copy pipeline, warp/lane mapping, fragment layouts, MMA tensorization, and immutable schedule records/witnesses. Acceptance: producer/consumer order, barriers, buffer rotation, bounds, target features, and witness mutations reject.
