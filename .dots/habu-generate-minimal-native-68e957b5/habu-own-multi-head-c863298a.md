---
title: Own multi-head attention state
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T23:33:37.511510+02:00"
---

maki/mha.f is a 172-line fixed-toy production surface used only by mha-test.f: five EXTENT and fourteen TENSOR declarations, six SPEC-generated contractions, six global scratch buffers (~1,024 bytes at the toy shape), three mutable origin cells and MHA-/ROW-* helpers all live in broad package MAKI; E-MHA-SHAPE is global and MHA-O-SCRATCH is public only so tests can inspect internals. Calls are non-reentrant and raw-pointer/capacity-free. MHA-SUBLAYER-FWD is alias-unsafe: if yb==xb, MHA-FWD overwrites X before the residual, then T-ADD! doubles the new output instead of adding the original input. MHA-CONFIG-CHECK also executes runtime branches over hardcoded compile-time constants. Create a real MHA package and explicit STRUCTURE config, parameter spans and owned workspace after unified lowering; validate shapes/capacities and either support in-place through preserved input or reject aliasing before writes. Keep scratch private and return stable inspection values from a test owner. Derive geometry statically instead of retaining guaranteed-false runtime checks. If the module remains only a golden, move it to focused test support; if production consumes it, export only one checked call. Add alias/overlap matrices, short spans, two independent workspaces, nested/concurrent calls, old-name/private-access negatives, exact numerics, mapping/canary checks and source/public-definition/JIT/DATA/CODELEN reduction. Files: maki/mha.f/tests and direct callers. Block STRUCTURE work on habu-lowering-hash-unified-586f7881; trainability and QKV semantics remain habu-complete-trainable-multi-39e26b3d.

2026-07-20 SERIALIZED behind habu-complete-trainable-multi-39e26b3d (spark lane running): same mha.f territory; state/packaging rework lands on the trainable surface.
