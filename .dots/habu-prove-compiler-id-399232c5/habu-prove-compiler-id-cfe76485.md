---
title: Prove compiler ID laws
status: active
priority: 1
issue-type: task
created-at: "2026-07-27T13:50:33.899776+02:00"
blocks:
  - habu-model-compiler-ids-bcc76d07
---

Claim: agent=proof_id_laws workspace=.jj-ws/proof-id-laws

Scope: prove only the bounded representation laws of the IR-0.1 Rocq ID model: a 31-bit serial plus 32-bit local pack stays at or below signed-cell max; rshift 32 and the low-32 mask recover the components; packing is injective; pack/projection roundtrip holds within bounds; owners separate; and negative/equal/overflow bounds reject in the exact 64-bit model. Separately prove that every valid `ir-count` and `ir-pool-offset` constructor/projector round-trip is raw scalar identity across the full 0..signed-cell-max domain and never introduces or removes module bits. Static wrong-family distinction is owned by Rocq type/schema parity plus the checked Habu negative, not a runtime kind predicate. Acceptance: Rocq 9.2 proves every named law with no Admitted; mutation examples fail when cell width, serial/local width, shift, mask, owner, bound, or scalar identity is weakened; assumptions remain explicit. Ownership: compiler-ID bounded representation theorem file(s) only; do not edit the checked manifest/vector artifact, allocator model/laws, or Ids.v syntax except through a separately reviewed dependency repair. Excludes shared records, tables, opcodes, general witnesses, dialects, native/GPU, and maki. Depends on habu-model-compiler-ids-bcc76d07.

Checkpoint:

1. Owner: Rocq module `Habu.Common.IdLaws`; sole model dependency `Habu.Common.Ids`.
2. Entry: `formal/Common/IdLaws.v`.
3. Green: `rocq compile -Q formal Habu formal/Common/Ids.v` succeeds.
4. Red: after Green, `rocq compile -Q formal Habu formal/Common/IdLaws.v` fails before the theorem file exists.
5. Interface: bounded pack, projection, injection, owner separation, reject, count scalar-identity, and pool-offset scalar-identity theorems plus focused counterexamples.
6. Forbidden: changing `Ids.v`, runtime kind predicates, examples as theorem substitutes, missing boundary mutations, scalar proof only below bit 32, allocator/parity work, or Admitted.
7. Focused: run `rocq compile -Q formal Habu formal/Common/Ids.v`, then `rocq compile -Q formal Habu formal/Common/IdLaws.v`.
8. Broader: every current `formal/Common/*.v`, no-Admitted and assumption scans, host, file-map, stale-status, and dot dependency gates.
