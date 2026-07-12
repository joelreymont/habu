---
title: "Nominal storage: effect parametricity"
status: open
priority: 2
issue-type: task
blocks:
  - habu-nominal-storage-raw-493b6c5d
created-at: "2026-07-12T15:48:44.696398+02:00"
---

Phase 2 of habu-checker-seal-nominal-0b2eaece after raw TV kinds. At definition certification, prove every declared quantified effect variable remains a distinct variable after body checking; reject specialization or aliasing to concrete families, atoms, pointers, layouts, or another declared quantifier. Preserve sound generic LOAD and ID wrappers. Emit E-NONPARAMETRIC-EFFECT with pointer-pointee path and repair class preserve_type_parameter. Cover ptr family to ptr a erasure across definitions, direct nominal to a erasure, injectivity, multi-error rollback, native verifier, snapshot and bootstrap parity.
