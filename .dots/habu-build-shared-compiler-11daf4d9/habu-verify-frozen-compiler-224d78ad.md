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

Obligations handed over by the operation/value pool lane (landed 2026-07-29).
IR-OP enforces operand, result and successor COUNTS against the opcode's
schema, including the variadic-tail rule, but deliberately leaves these to this
freeze verifier because design line 538 assigns them here:
- TYPE rules: an operand's value type and a result's declared type are not
  compared against the schema's declared operand/result type lists, and an
  operation's attributes are not matched against the schema's required
  attribute keys. Negative fixtures needed: wrong operand type, wrong result
  type, missing required key, unknown key on a schema without the extension
  flag set.
- SUCCESSOR EXISTENCE: IR-OP validates a successor block's owning module but
  not its existence, because a branch to a block still under construction is
  ordinary SSA construction. Check existence and expected kind here.
- VALUE/OPERATION AGREEMENT: a value row's (defining operation, position) pair
  is not checked against that operation's result window. Design section 6.5
  requires every value to have exactly one definition. Negative fixtures:
  a successor naming no block, and a value row whose defining operation
  disowns it.

