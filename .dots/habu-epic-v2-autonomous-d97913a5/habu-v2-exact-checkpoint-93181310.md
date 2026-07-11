---
title: V2 exact checkpoint resume
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T12:25:27.752328+02:00"
blocks:
  - habu-v2-experiment-run-7c1d1906
---

Implement checkpoint objects covering model, optimizer, scaler, RNG algorithm/state, data cursor, run key, compiler/schema versions, and parent checkpoint. Acceptance: injected interruption followed by resume produces the same next batch, update, metrics, and child digest as uninterrupted execution; incompatible schema/data/run key returns typed rejection; retention never deletes a promoted lineage root.
