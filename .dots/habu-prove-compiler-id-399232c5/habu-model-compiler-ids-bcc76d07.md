---
title: Model compiler IDs in Rocq
status: open
priority: 1
issue-type: task
created-at: "2026-07-27T13:50:23.724542+02:00"
---

Claim: agent=proof_ids workspace=.jj-ws/proof-ids (RELEASED 2026-08-21: workspace gone, no live lane - gc)

Scope: define formal/Common/Ids.v for the exact IR-0.1 identity vocabulary: ir-module-key, module/source/fun/block/op/value/type/attr/symbol/span IDs, pool offset, and count. Model exact signed 64-bit cells, the 31-bit positive serial, the 32-bit unsigned local index, bounded packing, rshift-32 and mask projections, and executable validity predicates using NEWTYPE/ENUM/STRUCTURE-equivalent syntax. Model `ir-count` and `ir-pool-offset` as separate scalar structures whose constructors and projections preserve the full nonnegative signed-cell value without packing, shifting, masking, owner, or local-index semantics. Wrong-family distinction is static in the Rocq/Habu schemas, not a fabricated runtime kind tag. Acceptance: Rocq 9.2 compiles; definitional examples cover valid and rejected numeric predicates, exact word bounds, and discriminating scalar values above bit 32; no theorem beyond definitional examples and no Admitted. Ownership: formal/Common/Ids.v syntax and executable predicates only, disjoint from checked manifest/vectors, allocator state, and later law/parity files. Excludes shared records, tables, opcodes, general witnesses, dialects, native/GPU, and maki. Depends on habu-add-compiler-ir-21e976fc.

Checkpoint:

1. Owner: Rocq module `Habu.Common.Ids`; corresponding production package `IR-ID`.
2. Entry: `formal/Common/Ids.v` under `rocq compile -Q formal Habu`.
3. Green: the accepted IR-0.1 focused suite on the verified prerequisite.
4. Red: `rocq compile -Q formal Habu formal/Common/Ids.v` fails because the module does not exist.
5. Interface: exact constants, one distinct structure per IR-0.1 family, packed-ID predicates/projections, and identity-preserving scalar constructors/projections.
6. Forbidden: runtime kind tags, unguarded unbounded-integer claims, one generic ID type, packed scalar families, allocator state, theorem work, or Habu edits.
7. Focused: `rocq compile -Q formal Habu formal/Common/Ids.v`.
8. Broader: every current `formal/Common/*.v`, no-Admitted and assumption scans, host, stale-status, and dot dependency gates.

BLOCKER RESOLVED 2026-08-04 (dot-purge): `habu-add-compiler-ir-21e976fc` is closed and its edge removed here. The IR-ID module landed - `src/compiler/ir/id.f`, `test/compiler/ir-id.f` and `test/compiler/ir-id-concurrency.f` are present on the `proofs` branch and `proofs@origin` is at the same revision (1bb76eab), which is exactly the close condition that dot recorded for itself. The dependency is satisfied, not dissolved.
