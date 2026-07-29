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

Further obligations handed over by the function/block lane (landed 2026-07-29).
IR-FUN deliberately stores neither predecessor-count nor successor-count
(design lines 404-405) because line 410 says those tables are DERIVED at freeze
rather than maintained through builder mutation — a stored count would be wrong
for as long as any branch to the block is unwritten. This verifier must:
- Walk every terminator's successor window, build the predecessor and successor
  tables, and record the counts.
- Check COVERAGE. IR-FUN's window tiling proves every operation up to the last
  block's end belongs to exactly one block, and every block up to the last
  function's window end belongs to exactly one function. Operations appended
  AFTER the last block, and blocks appended for a function later abandoned, lie
  outside every window; only comparing total coverage against IR-OP:OPS and
  IR-FUN:BLOCKS at freeze catches them.
- Check BLOCK ARGUMENT DEFINITIONS. ADD-BLOCK-ARG must mint a value row before
  the block's own row can exist, so a rejected END-BLOCK leaves argument values
  naming a block that does not exist (the design's answer is builder ABORT,
  section 6.2). Reject any blk-arg value whose block does not exist or whose
  block's argument window does not contain it — section 6.5's "every value has
  exactly one definition", made concrete for block arguments.
- Check SUCCESSOR EXISTENCE AND ARGUMENT AGREEMENT (carried from the operation
  lane, now possible): every successor must name an existing block, and
  successor argument counts and types must match the destination block's
  arguments. IR-FUN supplies BLOCK-COUNT and ARG@ for both.

