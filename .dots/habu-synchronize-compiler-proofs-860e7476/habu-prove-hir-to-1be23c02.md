---
title: Prove HIR to SIR refinement
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:04:26.741213+02:00"
blocks:
  - habu-compare-native-control-f8b5d801
  - habu-prove-shared-ir-1a95c6ef
---

Full context: design section 10.2 requires source/checker/HIR binding and stack-to-SSA semantic preservation for the covered straight/control slice. Consume the typed heap and separation model so HIR/SIR loads, stores, allocation, free, traps, and unknown effects carry exact address-space, lifetime, permission, alias, and footprint facts; simulation uses locality/frame rather than an implicit flat store. Full prerequisites include habu-prove-compiler-separation-db458ea0 and the recorded shared-IR/control edges. Acceptance: elaborated HIR and verified SIR preserve stack, value, control, and heap behavior; shared fixtures/witness vectors pass Habu and Rocq; no Admitted.
