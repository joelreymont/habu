---
title: source comments cite unpinned Examples as machine-checked facts
status: open
priority: 3
issue-type: task
created-at: "2026-08-22T22:38:26.070155+02:00"
---

Problem: src/compiler/ir/type.f:93-98 and attr.f:116-120 cite Types.ty_both_orders_admissible (Interning.v:1581) and ty_denotation_order_independent (:1653) - both Examples, neither in ir-intern-axioms.txt, and Interning.v:1557-1559 says the universal form is MODEL GAP 8; type.f's comment does not name the gap. Acceptance: promote the two into the manifest or reword the comments to 'exhibited on one pair'. Files: src/compiler/ir/type.f, attr.f, test/compiler/ir-intern-axioms.txt. Verify: proof slice. Depends: prover. Ownership: proofs. Claim: unassigned.
