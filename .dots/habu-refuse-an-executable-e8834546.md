---
title: Refuse an executable value fetched through raw storage
status: active
priority: 2
issue-type: task
created-at: "2026-09-18T12:50:13.914458+03:00"
---

Problem (adversarial review of the 2026-09-18 batch, probes p27-p30; PRE-EXISTING, verified on raw-rule-gen3): a DATA word can be declared to hold a quotation and executed — ': QCELL ( -- ptr [ -- n ] ) data-base 8 + ;' certifies, ': FIRE ( -- n ) QCELL @ execute ;' certifies (the fetched value carries a declared quotation type, so MD-EXEC-OPAQUE never fires), and '1 W FIRE' branches to address 1 (SIGBUS); 'variable ZQW : ZQWQ ( -- ptr [ -- n ] ) ZQW ; : I1 ( -- n ) ZQWQ @ execute' certifies on both engines; NULL-PTR 8 + the same. Cause: FENCE-WHY's admissibility (inherited from RAW-OK?) admits an atom / xt / row payload because the engine raw-stores those, so an undeclared cell may be declared to hold an xt or quotation and the opaque-execute rule judges opacity, not provenance. Acceptance: a T-QUOT or xt payload reached through an undeclared raw cell or a base address is refused by name (E-RAW-CELL-PTR family, own reason, repair class declare_pointer_cell or a new declare_xt_cell), while the declared forms (TYPED-VARIABLE NAME [ a -- b ], TYPED-BUFFER, defer/is, xt!) keep certifying; red-first cases in test/compiler/raw-cell-pointer-refusals.f and base-pointer-arith-refusals.f; a sweep of lib/ tools/ test/ for the shape with the count recorded; docs/effects.md. Files: src/core/checker.f, src/core/render.f, the fixtures, docs/effects.md. Verify: the fixtures; three generations with cmp; test/run.f. Depends: habu-fence-a-base-c6c1d71d. Ownership: checker. Parent: habu-campaign-c2-mem-c3d7662b. Claim: agent=hazel workspace=.jj-ws/hazel-exec-cell.
