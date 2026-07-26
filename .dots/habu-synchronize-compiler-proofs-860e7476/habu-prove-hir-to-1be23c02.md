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

Full context: design section 10.2 requires source/checker/HIR binding and stack-to-SSA semantic preservation for the covered straight/control slice. Acceptance: elaborated HIR and verified SIR simulations preserve stack/value/control semantics; shared fixtures/witness vectors pass Habu and Rocq; no admitted cases.
