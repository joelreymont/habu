---
title: "Migration: core records to STRUCTURE"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:46:05.263733+02:00"
blocks:
  - habu-compiler-lower-unified-5f599080
---

Mechanically migrate src/core and src/habu declarations from BEGIN-STRUCTURE/END-STRUCTURE, VALUE-RECORD, and PRODUCT to typed STRUCTURE ... ;STRUCTURE. Preserve field offsets, pointer roles, generics, visibility, effects, snapshots, and ABI layouts exactly. Convert consumers to generated package APIs and typed accessors; remove raw prefixed access where package fields exist. Run exact core loads, engine suites, source certification, typed-local diff lint, trust lint, and fixpoint stage gate.
