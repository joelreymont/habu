---
title: "Migration: core records to STRUCTURE"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:46:05.263733+02:00"
blocks:
  - habu-compiler-lower-unified-5f599080
  - habu-core-records-remove-0d8ff4e2
---

Mechanically migrate src/core and src/habu declarations from BEGIN-STRUCTURE/END-STRUCTURE, VALUE-RECORD, and PRODUCT to typed STRUCTURE ... ;STRUCTURE. Preserve field offsets, pointer roles, generics, visibility, effects, snapshots, and ABI layouts exactly. Convert consumers to generated package APIs and typed accessors; remove raw prefixed access where package fields exist. Run exact core loads, engine suites, source certification, typed-local diff lint, trust lint, and fixpoint stage gate.

Bootstrap correction: pre-checker implementation records cannot use the typed
public STRUCTURE definer without a cycle. The child dots replace those internal
layouts with named offset/size/alignment constants plus assertions. Typed
STRUCTURE remains the sole public composite-record syntax and loads only after
checker/type-family initialization; no private or raw record DSL survives.
