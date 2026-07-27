---
title: Compile native straight-line slice
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:56:29.030775+02:00"
blocks:
  - habu-record-native-slice-a01a8ad7
---

Full context: design Wave 2 requires one real checked straight-line definition through source tape, HIR, stack SSA, LIR, A64IR, allocation, bytes, isolated execution, and shadow comparison. Acceptance: SQUARE/arithmetic/stack-renames match the old path; pure renames emit no instructions; maps/dumps/coverage/metrics are complete; old path remains sole publisher.
