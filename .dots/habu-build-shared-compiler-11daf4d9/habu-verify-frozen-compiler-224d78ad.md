---
title: Verify frozen compiler IR
status: open
priority: 1
issue-type: task
created-at: "2026-07-26T22:55:16.416248+02:00"
blocks:
  - habu-freeze-compiler-ir-6f706100
---

Full context: design sections 5.8, 6.5, and 16.1 require independent structural validation before publication. Implement a verifier separate from producers for owners, bounds, windows, parents, definitions, terminators, successors, dominance, schema, attributes, effects, symbols, spans, target legality, and placeholders. Acceptance: one hostile mutation fixture per invariant produces a named module/function/block/op/span diagnostic.
