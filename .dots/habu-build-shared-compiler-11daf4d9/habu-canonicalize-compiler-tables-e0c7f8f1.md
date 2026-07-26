---
title: Canonicalize compiler tables
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:55:16.430122+02:00"
blocks:
  - habu-verify-frozen-compiler-224d78ad
---

Full context: design sections 5.7 and 6.6 require construction-order-independent bytes without reordering semantic control flow. Sort strings first, then dependency-order symbols, types, attributes, and sources; rewrite every reference while preserving function/block/op/operand/result/successor order. Acceptance: equivalent modules with reversed intern insertion encode/digest identically; semantic order changes remain observable. Dependency: frozen verifier.
