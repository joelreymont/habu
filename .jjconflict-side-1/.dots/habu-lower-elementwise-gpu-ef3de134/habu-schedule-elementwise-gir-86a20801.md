---
title: Schedule elementwise GIR
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:02:29.224780+02:00"
blocks:
  - habu-build-elementwise-kir-a7d6a029
---

Full context: lower elementwise KIR to simple flat GPU-GIR with explicit thread/block mapping, index math, memory order, target contract, and schedule witness. Acceptance: broadcast/domain/address maps, bounds, target limits, and witness mutations reject; equivalent schedules encode deterministically.
