---
title: Canonicalize compiler tables
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:55:16.430122+02:00\""
blocks:
  - habu-verify-frozen-compiler-224d78ad
---

Full context: design sections 5.7 and 6.6 require construction-order-independent bytes without reordering semantic control flow. Sort strings first, then dependency-order symbols, types, attributes, and sources; rewrite every reference while preserving function/block/op/operand/result/successor order. Acceptance: equivalent modules with reversed intern insertion encode/digest identically; semantic order changes remain observable. Dependency: frozen verifier.

Evidence 2026-07-28 (from formal/Common/Interning.v): the "rewrite every
reference" clause above is not optional bookkeeping — it is load-bearing, and
there is now a machine-checked counterexample proving a bare permutation is
wrong. A pointer row stores its pointee's module-local ORDINAL
(src/compiler/ir/type.f POINTER: `r ptee ID-CK` then INTERN4 stores it), and a
function-type row stores a pool window of ordinals, so reordering the type
table changes stored row content. Theorem Types.ty_both_orders_admissible
builds i8, i16 and pointer-to-i8 in the two admissible orders and gets rows
[i8; i16; TPtr 0 0] versus [i16; i8; TPtr 0 1]; Types.structural_rows_not_permutation
proves those are NOT a permutation of each other, while
Types.ty_denotation_order_independent shows the denotations do agree — which is
the property this encoder must target. IR-ATTR is worse: attr.f ORD-OK admits
foreign-table ordinals (symbol and type ids) into attribute rows, so the
encoder must renumber under the attribute permutation AND under the symbol and
type permutations.

A second unstated premise this encoder depends on: not every insertion order is
admissible. POINTER rejects a pointee ordinal not already below the live count,
so the admissible build orders are exactly the topological orders of the
reference DAG. "Any two orders" means "any two topological orders" for the type
and attribute tables.


Claim: agent=ircanon workspace=.jj-ws/habu-canonicalize-compiler-tables-e0c7f8f1
