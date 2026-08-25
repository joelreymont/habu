---
title: "ENUM: infer canonical layout"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:14:14.889278+02:00"
blocks:
  - habu-enum-finalize-family-4f4333b0
---

Own ENUM family-kind/layout finalization and focused layout tests. Infer internal tag-only enum only when every variant field range is empty; otherwise infer internal tagged sum. Preserve runtime cell order, width, alignment, linear payload rules, POLICY, DERIVE, snapshot identity, and canonical hashes.
