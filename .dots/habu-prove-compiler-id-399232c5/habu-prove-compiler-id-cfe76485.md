---
title: Prove compiler ID laws
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T13:50:33.899776+02:00"
blocks:
  - habu-model-compiler-ids-bcc76d07
---

Scope: prove only the bounded representation laws of the IR-0.1 Rocq ID model: a 31-bit serial plus 32-bit local pack stays at or below signed-cell max; rshift 32 and the low-32 mask recover the components; packing is injective; pack/projection roundtrip holds within bounds; owners separate; and negative/equal/overflow bounds reject in the exact 64-bit model. Static wrong-family distinction is owned by Rocq type/schema parity plus the checked Habu negative, not a runtime kind predicate. Acceptance: Rocq 9.2 proves every named law with no Admitted; mutation examples fail when cell width, serial/local width, shift, mask, owner, or bound is weakened; assumptions remain explicit. Ownership: compiler-ID bounded representation theorem file(s) only; do not edit the checked manifest/vector artifact, allocator model/laws, or Ids.v syntax except through a separately reviewed dependency repair. Excludes shared records, tables, opcodes, general witnesses, dialects, native/GPU, and maki. Depends on habu-model-compiler-ids-bcc76d07.
