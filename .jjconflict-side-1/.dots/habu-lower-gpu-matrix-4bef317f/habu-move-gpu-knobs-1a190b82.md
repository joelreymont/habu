---
title: Move GPU knobs to schedules
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:03:22.174036+02:00"
blocks:
  - habu-port-known-good-9b32d5fa
---

Full context: after the known-good path passes, move BM/BN/BK, warps, stages, swizzle, layouts, and epilogue choices from hand emitters into immutable validated schedule records. Acceptance: each knob participates in schema/digest/witness/resource validation; current search space reproduces the winner before any expansion.
