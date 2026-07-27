---
title: Model compiler IDs in Rocq
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T13:50:23.724542+02:00"
blocks:
  - habu-add-compiler-ir-21e976fc
---

Scope: define formal/Common/Ids.v for the exact IR-0.1 identity vocabulary: ir-module-key, module/source/fun/block/op/value/type/attr/symbol/span IDs, pool offset, and count. Model exact signed 64-bit cells, the 31-bit positive serial, the 32-bit unsigned local index, bounded packing, rshift-32 and mask projections, and executable validity predicates using NEWTYPE/ENUM/STRUCTURE-equivalent syntax. Wrong-family distinction is static in the Rocq/Habu schemas, not a fabricated runtime kind tag. Acceptance: Rocq 9.2 compiles; definitional examples cover valid and rejected numeric predicates and exact word bounds; no theorem beyond definitional examples and no Admitted. Ownership: formal/Common/Ids.v syntax and executable predicates only, disjoint from checked manifest/vectors, allocator state, and later law/parity files. Excludes shared records, tables, opcodes, general witnesses, dialects, native/GPU, and maki. Depends on habu-add-compiler-ir-21e976fc.
