---
title: Verify frozen compiler IR
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-26T22:55:16.416248+02:00\""
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


Claim: agent=irverify workspace=.jj-ws/habu-verify-frozen-compiler-224d78ad (RELEASED 2026-08-21: workspace gone, no live lane - gc)

## Result (irverify, measured)

What landed, in three reviewed commits.

**Commit A - `Key operation attributes by schema key symbol`** (prerequisite dot
habu-key-op-attrs-6e19c813). `IR-OP:ADD-ATTR` was `( ir-attr-id -- )`, so an
operation stored attribute values with no keys at all, while a schema declares
required attribute KEYS. Matching one against the other was therefore not just
unchecked but unstatable. `ADD-ATTR` is now
`( ir-symbol-id ir-attr-id -- )`, an attribute entry occupies two pool cells
(the key symbol ordinal then the attribute ordinal, named by `AT-CELLS`), the
stored count stays a count of attributes, and the window tiling multiplies by
the stride in the four places that convert the window to cells. New readers
`ATTR-KEY@` and `FATTR-KEY@` serve the key. `IR-BUILD:ADD-ATTR` gained the key
argument. The key is owner-checked and non-negative in IR-OP and its existence
is left to this verifier, which is the same division IR-OP already makes for a
successor's block, because IR-OP holds no symbol table.

**Commit B - `Prove context ownership when freezing a module`.**
`IR-BUILD:FREEZE` is now `( IR-CTX:ctx IR-BUILD:builder -- IR-BUILD:module )`
and passes the same `USE` gate every append does. Two independent reasons: the
verifier needs the bound target contract, which only the context holds, and it
needs an allocator for the derived table; and publication was the one state
change in IR-BUILD that did not prove the caller owns the compilation. All 21
call sites in `test/compiler/ir-build.f` were migrated, including the
checker-sealing fixture, and a new fixture proves that freezing with a live but
foreign context is refused with `E-IR-BUILD-OWNER`.

**Commit C - `Verify frozen compiler IR before publication`.**
`src/compiler/ir/verify.f` opens package IR-VERIFY. `IR-BUILD:FREEZE` calls
`IR-VERIFY:VERIFY` as its last refusal arm, after the ceiling and liveness
checks and before `TABLES-FREEZE`, so a refused freeze still publishes nothing;
`build.f` contains no verifier logic, only the one call that hands over the
module's tables.

Ordering matters and is documented in both files: `IR-ARENA:FREEZE` cannot be
undone, so a verifier running after it would leave a builder whose tables are
frozen and whose slot is still live - unpublished but no longer correctable.
Every check therefore reads the live builder arenas.

**The derived tables.** A module now has seventeen tables rather than fifteen:
a predecessor pool and a block-edge row table, created in `NEW-BUILDER` with
ceilings read off the same committed plan (one row per block, one pool cell per
operand-pool cell, which bounds the edges because every successor occupies one
of those cells). One row per block holds the predecessor window and the
successor count - design lines 404 and 405, the two fields IR-FUN deliberately
does not store. They are published through the frozen-module surface as
`IR-BUILD:FEDGE-POOL` and `FEDGE-ROWS` and read with
`IR-VERIFY:FEDGE-BLOCKS`, `FPRED-COUNT`, `FSUCC-COUNT` and `FPRED@`. The
frozen module is the natural owner because a later pass must get these facts
from the module it was handed, not from a pass-local cache that could disagree
with it.

The successor LIST is deliberately not copied. It is already stored exactly
once, in the terminator's successor window, and IR-OP is its single authority;
copying it would create a second authority for a fact the module already holds,
which is the same rule that kept parent-block out of the operation row. What is
genuinely derived is the inverse relation and the two counts.

**Dominance** is the ordinary iterative dataflow over the derived predecessor
table: the entry dominates only itself, every other block starts dominated by
all of its function's blocks, and each round intersects the predecessors' sets
and adds the block itself until nothing changes. The sets only shrink, so it
terminates. Blocks of one function are contiguous ordinals because the window
tiling makes them so, which is what lets the fixpoint run per function. An
unreachable block keeps the full set, the standard initialisation.

**The committed working set.** The dominator sets and predecessor lists are
package-owned arrays bounded by `BLOCK-MAX` 256 - the same number the
production plan commits to for blocks - and `EDGE-MAX` 2048, in the shape
IR-OP's `ARITY-MAX` and IR-FUN's `ATTR-MAX` already use. A module planned
larger is refused by name with `E-IR-VERIFY-CAP` rather than checked partially.

**Error codes.** The block -8080..-8099 is claimed whole in `lib/errors.f` as
E-IR-VERIFY-STATE, -OWNER, -BOUND, -CAP, -COVER, -PARENT, -TERM, -SUCC,
-SUCCARG, -DEF, -ARGDEF, -DOM, -SCOPE, -OPTYPE, -RESTYPE, -ATTRKEY, -EFFECT,
-SYMBOL, -SPAN and -TARGET - all twenty. Every arm throws its own code rather
than letting a producer's reader throw first, so a refusal names the invariant
that broke and not the table that noticed.

### Fixture to diagnostic

Each row is one hostile module built through the real IR-BUILD API with exactly
one invariant corrupted, in `test/compiler/ir-verify.f`.

| Fixture | Corruption | Diagnostic |
| --- | --- | --- |
| SUCC-CASE | branch to a block ordinal never built | E-IR-VERIFY-SUCC |
| SUCCARG-CASE | branch hands its one-argument destination none | E-IR-VERIFY-SUCCARG |
| SUCCTYPE-CASE | branch hands an i64 to an i32 argument | E-IR-VERIFY-SUCCARG |
| ARGDEF-CASE | block argument minted, then the block abandoned | E-IR-VERIFY-ARGDEF |
| OPCOVER-CASE | an operation appended after the last block | E-IR-VERIFY-COVER |
| BLKCOVER-CASE | a block left behind by an abandoned function | E-IR-VERIFY-COVER |
| OPTYPE-CASE | an i32 operand where the schema declares i64 | E-IR-VERIFY-OPTYPE |
| RESTYPE-CASE | an i32 result where the schema declares i64 | E-IR-VERIFY-RESTYPE |
| MISSKEY-CASE | a required attribute key the operation omits | E-IR-VERIFY-ATTRKEY |
| EXTRAKEY-CASE | a key the opcode never declared, no extension set | E-IR-VERIFY-ATTRKEY |
| EFFECT-CASE | a memory effect with no token type to carry it | E-IR-VERIFY-EFFECT |
| DOM-CASE | a use in a block the definition does not dominate | E-IR-VERIFY-DOM |
| SCOPE-CASE | an operand defined in another function | E-IR-VERIFY-SCOPE |
| PREDIDX-CASE | a predecessor index past the derived count | E-IR-VERIFY-BOUND |
| LEGAL-CASE | none - a well-formed module freezes | passes |
| EDGE-CASE | none - a diamond's derived counts read back | passes |
| REFUSE-CASE | a refused freeze publishes nothing | E-IR-VERIFY-SUCC, then a legal module freezes |

### Arms with no fixture, and why

These are the invariants no checked caller can break, so the arm is defense in
depth against a forged or corrupted table rather than against a caller. The
unreachability is the result; each was traced to the producer that already
enforces it.

- `E-IR-VERIFY-TERM` - `IR-FUN:END-BLOCK` already requires exactly one
  terminator and that it is the block's last operation, before the block row
  exists.
- `E-IR-VERIFY-PARENT` - `IR-FUN:END-FUN` makes the parent field and the block
  window agree, and `BLOCK@` rechecks on every read.
- `E-IR-VERIFY-DEF` - result values are minted by `IR-OP:END-OP` itself from
  the same append, so no caller can write a value row that its operation
  disowns.
- `E-IR-VERIFY-SPAN` - IR-OP and IR-FUN both run `IR-SOURCE:SPAN-CK` at append.
- `E-IR-VERIFY-SYMBOL` - `IR-FUN:END-FUN`'s duplicate check already refuses a
  second function of the same name, and every producer validates the symbols it
  is handed.
- `E-IR-VERIFY-TARGET` - `IR-SCHEMA:DEFINE` refuses an opcode the bound
  contract cannot execute, and a context's binding cannot change afterwards.
- `E-IR-VERIFY-CAP` for the block ceiling - reaching it needs a module with more
  than 256 blocks, which does not fit the test-scale context mapping. The
  smaller derived-table ceiling arm is covered indirectly by `ROOM-CK`.
- `E-IR-VERIFY-STATE` and `E-IR-VERIFY-OWNER` on the derived table's frozen
  readers - a forged view cannot be spelled in checked code.
- `E-IR-OP-STATE` for an attribute-window length that is not a whole number of
  entries, added in commit A. Measured, not assumed: deleting that line leaves
  `ir-op.f` and `ir-structure-proof.f` both green, so the arm is genuinely
  uncovered. It cannot be isolated with the existing forge harness, because a
  forged row's windows all start at the end of the live pool, so any positive
  attribute length overflows the pool and is refused by the pool bound before
  the parity of the length is ever examined. Covering it needs a forge harness
  that can place a row with pool cells to spare after it; that is a test-support
  capability, and the arm stays defense in depth until it exists.

### MODEL GAPS

- **Per-successor arguments.** An operation has one operand list and a
  successor list, with no way to say which operands belong to which successor.
  Argument agreement is therefore only decidable when a terminator has exactly
  one successor, which is what the verifier checks; a conditional branch with
  two destinations cannot state its arguments at all today. This needs an
  operation-model change (a per-successor argument window) and is not something
  the verifier can work around.
- **A branch must stay inside its function.** Nothing checks that a successor
  block belongs to the same function as the branch. The dominance arm rejects a
  cross-function OPERAND with `E-IR-VERIFY-SCOPE`, but a cross-function
  successor with no operands would pass. This is a missing invariant, not a
  missing fixture.
- **The verifier's working set is a committed ceiling, not a growing array.** A
  module with more than 256 blocks is refused rather than verified.

### Measured gate results

All on this workspace, over parent dac816a7.

- `./bin/hb --load test/compiler/ir-verify.f` - exit 0, `test: ok`, 24 cases.
  Before the module existed the same path failed with exit 74,
  `include: open failed`.
- `./bin/hb --load test/compiler/ir-build.f` - exit 0, `test: ok`.
- `./bin/hb --load test/compiler/ir-op.f` - exit 0, `test: ok`.
- `./bin/hb --load test/compiler/ir-fun.f` - exit 0, `test: ok`.
- `./bin/hb --load test/compiler/ir-schema.f` - exit 0, `test: ok`.
- `./bin/hb --load test/compiler/ir-arena.f` and `ir-context.f` - exit 0 each.
- `./bin/hb --load tools/error-code-lint.f` - exit 0,
  `1326 file(s), 875 claim(s), 39 reservation(s), 0 finding(s)`
  (855 claims before; the twenty new codes are the difference).
- `./bin/hb --load tools/suite-coverage-lint.f` - exit 0,
  `166 suite(s), 0 finding(s)` (165 before).
- `./bin/hb --load tools/package-diff-lint.f -- <jj diff --git>` - exit 0 on
  each of the three commits' artifacts.
- `./bin/hb --load tools/typed-local-diff-lint.f -- <same artifacts>` - exit 0
  on each.

### Mutation evidence

Seven mutations of the verifier, each restored and green afterwards:

- successor existence reduced to a negativity test - suite red (2 failures).
- operation coverage total neutralised - suite red (1 failure). Written as
  `OPS 0 <` first, which the checker itself rejected as a stack-effect error.
- dominance bit test neutralised - suite red.
- required-attribute-key count test neutralised - suite red.
- operand type comparison neutralised - suite red.
- successor argument count comparison neutralised - suite red.
- attribute pool written value-then-key instead of key-then-value - both
  `ir-op.f` and `ir-build.f` red.
- `ROW-ADD` writing the attribute ENTRY count instead of the window's cell
  length - `ir-op.f` red with an uncaught `E-IR-OP-STATE`, and
  `ir-structure-proof.f` red on the frozen body. This is the mutation that
  proves the representation decision is load-bearing in both directions.
- the odd-attribute-length refusal deleted - nothing red. Recorded above as an
  uncovered arm rather than reported as covered.

Two real bugs were found this way and fixed before the commit: the derived
pool writer and the predecessor filler each used the inner loop index where the
outer one was meant, so the predecessor lists were written against the wrong
block. The diamond fixture is what exposed both.

### Is this the best long-term solution, or a patch?

Long-term correct, with three named limits recorded above rather than hidden.
The verifier re-derives every fact from the tables instead of trusting a
producer's summary, which is what makes it an independent check; it throws its
own named code per invariant instead of surfacing the reader that noticed; and
the derived tables are published through the module rather than returned to the
freezing caller, so no later pass can hold a private copy that disagrees. The
two interface changes it forced (the keyed attribute and the context at freeze)
were both taken as structural fixes rather than worked around: the alternative
for attributes - inferring keys from record-valued attributes - would have left
any operation free to carry an unkeyed attribute the schema never declared,
which is a same-type semantic-role gap and exactly the kind of patch the review
gate exists to catch.

### One repair folded back into commit A

The first shape of the keyed attribute stored the ATTRIBUTE COUNT in the row's
attribute-window length field and multiplied by the stride in the tiling
arithmetic. `test/compiler/ir-structure-proof.f` caught it: that fixture freezes
the exact source text of `IR-OP:TILE-CK` and `FTILE-CK` because
`formal/Common/Structure.v` is proved against those bodies, and the model's
window length `wlen` is a length in POOL CELLS. Storing an entry count would have
made the stored field mean something different from the other three windows and
made the model's contiguity claim false of the shipped row - a silent proof
break, not a cosmetic diff.

The fix is the right way round: the row stores the attribute window's CELL
length, so `TILE-CK`, `FTILE-CK`, `ROW-END` and `FROW-END` are byte-identical to
what the model was proved against, the stride appears only where an entry is
addressed, and the entry count is the reader's division. `ROW-ADD` now writes
`L-AT SN@ AT-CELLS *`, which is that cell length; `ROOM-CK` already counted the
same product through `STAGED-CELLS`, which is what keeps the written length
inside the pool. A stored length that is not a whole number of entries is
refused with `E-IR-OP-STATE` rather than rounded down. The one frozen body that
did change is `ROW-ADD`, updated in `test/compiler/ir-structure-schema.f` with a
note recording why the model still holds. This was squashed into commit A rather
than added on top, because it is that commit's own concern.

### The gate-stdlib red set

Measured on the integrated tree. The brief's expected six
(engine-error-package, pre-trust-defer, aot-wid-restore,
stdlib-process-fixtures, owner-wid-internal, build-fixpoint-fixtures) did not
reproduce on this workspace at all: none of those six appeared in either run.
The first pooled run reported three reds - `compiler-ir-structure-proof`,
`compiler-ir-id` and `check-cli-boundary`. The first was mine and is fixed as
described above; the second pooled run on the integrated tree reports two reds,
the same two, and `compiler-ir-structure-proof` is green.

Both remaining reds pass standalone on this exact tree (`test/compiler/ir-id.f`
exit 0 `test: ok`; `tools/check-test.f` exit 0 `check-test: ok`) and both fail
in the pool on cases that have nothing to do with this work.
`compiler-ir-id` fails on `barrier-removal mutation fails overlap witness` and
`activation cleanup permits same-process task reuse`, which are concurrency
timing fixtures. `check-cli-boundary` fails inside the command-line capacity and
missing-source paths with an uncaught -2502. Neither touches attributes, the
freeze lifecycle, or the verifier. They are contention artifacts of a pool
running on a machine loaded with parallel agents, in the same class as the
TIMEOUT-UNDER-LOAD reds the brief warned about even though these surface as
plain exit codes. I could not reproduce the brief's six-red baseline to compare
against, so the honest statement is: no suite is red on this tree that is red
because of this work, and the baseline the brief quoted does not describe this
workspace. Confirming the six against a quiet machine is the integrator's call.

Blocker sweep 2026-08-21 (tracker GC): the blocks: list is gone because every entry in it was already closed - habu-freeze-compiler-ir-6f706100. The prose above still names them as prerequisites; they are satisfied, and nothing in the tracker blocks this leaf now.
