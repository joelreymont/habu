---
title: Prove shared IR validation
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T23:04:26.727505+02:00"
blocks:
  - habu-freeze-compiler-proof-e9f70b43
  - habu-prove-compiler-arena-59a8a885
  - habu-bind-compiler-id-596761f1
---

Full context: formalize shared module well-formedness, owner/bounds/windows/schema/freeze invariants, canonical codec relation, and the executable structural validator. Consume `Common/Memory.v`, `Common/Separation.v`, and `Common/Arena.v`: module windows name live owned allocations, builder mutation has the unique permission, abort/release consume it once, and freeze returns framed read-only storage. Full prerequisites: habu-freeze-compiler-proof-e9f70b43, habu-prove-compiler-arena-59a8a885, and habu-bind-compiler-id-596761f1. Acceptance: soundness covers every structural reject and ownership class; valid/corrupt vectors agree between Habu and Rocq; assumptions are explicit.
