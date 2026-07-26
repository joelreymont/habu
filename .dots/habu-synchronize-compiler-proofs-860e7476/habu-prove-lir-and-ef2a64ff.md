---
title: Prove LIR and A64IR lowering
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:04:26.768107+02:00"
blocks:
  - habu-lower-native-exceptions-6ceb7667
  - habu-prove-stable-sir-91b9d57d
---

Full context: formalize representation/ABI lowering, instruction selection, allocation-witness validation, call clobbers, stack homes, and control flow for covered native slices. Acceptance: composed refinement reaches allocated A64IR under target contract; invalid allocation/layout vectors reject in both validators.
