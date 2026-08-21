---
title: Drive unbound storage guards from vectors
status: open
priority: 2
issue-type: task
created-at: "2026-07-29T20:36:18.208683+02:00"
---

Full context: the substrate vacuity audit found three code mutations that leave the storage/structure gates fully GREEN — gate gaps, not proof vacuity; the theorems are fine, the vector tables lack rows. (1) IR-ARENA:ABORT is never exercised: deleting the slot-retirement line leaves ir-storage-proof green; add an OP-ABORT step to the vector vocabulary in test/compiler/ir-storage-schema.f, drive it in ir-storage-cases.f, emit the matching obligation, so an index minted before an abort is refused on both sides. (2) IR-CTX:DEPTH-ROOM never exercised: raising its bound past DEPTH-MAX stays green; add a row nesting to DEPTH-MAX requiring E-IR-CTX-DEPTH at the next entry. (3) Five guards pinned by frozen text only, never behaviourally: ROLLBACK's foreign-mark generation compare, WIN-STARTS window separation, TILE-CK's operand-window arm, ARGS-CK's this-block arm — add a cross-arena rollback row, an operation-pool window row, and an ARGS-CK wrong-block row. BINDING GAPS sections in the three model files record the details.

Claim: agent=storerows workspace=.jj-ws/habu-drive-unbound-storage-30b07943 (RELEASED 2026-08-21: workspace gone, no live lane - gc)

## Measured report — storerows

### What the gate could not see before

The substrate vacuity audit found that three classes of storage and structure
guard could be broken in the shipped source without either parity gate
noticing. Those gaps are now closed by rows in the shared vector tables, and
each closure was checked the way the gap was found: break the code, watch the
gate turn red on the new row, put the code back, watch it turn green again.

### The rows that were added

Storage (`test/compiler/ir-storage-schema.f`, one copy, read by both the Habu
cases file and the generated Rocq obligations):

- An `abort` operation was added to the arena step vocabulary, and an `abort`
  row pushes a cell, keeps its index, reads it, aborts the arena, and requires
  the same read to be refused afterwards, while the second arena carries on
  working. On the Rocq side the generated step machine now resolves its arena
  first and refuses everything on a retired one, which is what
  `IR-ARENA:RESOLVE` does; the model gained one definition, `alive`, for that.
- Two nesting depth rows were added, with their own small table. One opens 63
  contexts one inside another and requires the next entry to be accepted; the
  other opens 64 and requires the next entry to be refused by name with
  `E-IR-CTX-DEPTH`. The Habu side opens real contexts through
  `IR-CTX:WITH-CONTEXT`; the Rocq side asks the same question of the model's
  own nesting, through a new published result,
  `ctx_nesting_stops_at_depth_max`.
- A `foreign_mark` row rolls one arena back with a mark the other arena
  minted, requires `E-IR-ARENA-OWNER`, and then shows the first arena's cursor
  and cells untouched and its own mark still accepted.

Structure (`test/compiler/ir-structure-cases.f`): every accepted operation in
a value-store sequence now has its operands read back out of the cell pool
through `IR-OP:OPERAND@`, which revalidates the row's whole window tiling
before it reads a cell, and the ordinals that come back are compared with the
ones the shared row already records. No new table column was needed — the row
already carries the operands.

### The falsification matrix

Every line was run as `./bin/hb --load <gate>` from this workspace. "Green"
means exit 0 and `test: ok`. Where a guard was previously pinned only by its
frozen text, the mutation was applied twice: once to the code alone, and once
to the code together with the frozen text updated to match, so that only
behaviour could catch it.

| # | Guard | Mutation | Gate | Result |
|---|-------|----------|------|--------|
| 0 | — | none (baseline) | ir-storage-proof | green, exit 0, 8.8s |
| 1 | `IR-ARENA:ABORT` | slot retirement line deleted (`0 slot AGEN!` gone) | ir-storage-proof | RED, exit 1, 1 failure: the abort row's later read expected `-6650` (`E-IR-ARENA-STALE`) and got `0` |
| 1r | | restored | ir-storage-proof | green, exit 0 |
| 2 | `IR-CTX:DEPTH-ROOM` | bound raised (`DEPTH-MAX 1+ >=`) | ir-storage-proof | RED, exit 1: the depth-64 row expected `-6641` (`E-IR-CTX-DEPTH`) and got `0` |
| 3 | `IR-CTX:DEPTH-ROOM` | bound lowered (`DEPTH-MAX 1- >=`) | ir-storage-proof | RED, exit 1, 4 failures: the depth-63 row's entry was refused, and the depth-64 row could not even finish nesting |
| 3r | | restored | ir-storage-proof | green, exit 0 |
| 4 | `IR-ARENA:ROLLBACK` | generation compare deleted, code only | ir-storage-proof | RED, exit 1, 7 failures: the frozen guard body plus six steps of the `foreign_mark` row |
| 5 | `IR-ARENA:ROLLBACK` | same, with the frozen guard text updated to match | ir-storage-proof | RED, exit 1, 6 failures, all of them the `foreign_mark` row and none of them the frozen text |
| 5r | | restored | ir-storage-proof | green, exit 0 |
| 6 | — | none (baseline) | ir-structure-proof | green, exit 0 |
| 7 | `IR-OP:WIN-STARTS` | result window given the operand window's start, with the frozen guard text updated to match | ir-structure-proof | RED, exit 1, 7 failures, all of them the operand read-back (`E-IR-OP-WINDOW`, `-8028`) |
| 8 | `IR-OP:WIN-STARTS` + `IR-OP:TILE-CK` | operand start shifted by one cell AND the operand-window arm of the tiling check dropped, both frozen texts updated to match | ir-structure-proof | RED, exit 1, 4 failures, all of them the operand read-back handing back the wrong cell |
| 8r | | restored | ir-structure-proof | green, exit 0 |

Rows 4 and 5 are the pair that matters for the "pinned by text only" class: on
its own, mutation 4 would have been caught by the frozen body alone, which is
exactly the weakness the audit reported. With the text re-frozen (row 5) the
gate still goes red, and only the new vector row is what catches it. The same
pairing is why mutation 8 combines two edits: dropping the tiling check's
operand arm alone is invisible because the builder never writes a wrong
operand start, so the writer had to be moved as well for the missing check to
have anything to miss.

### Slowness found and fixed on the way

The first working version of the depth obligation asked Rocq to compute the
nesting directly. That took 65 seconds per row, because the model counts in
`nat`, the generation ceiling is 2147483647, and any computation that forces
that number into constructor form has to build two billion successors. With
two rows the whole gate ran past the proof assistant's own timeout and the
gate then reported that nothing had been proved. The fix was to prove the
nesting once and for all in the model, as `ctx_nesting_stops_at_depth_max`,
and have each row instantiate that theorem instead of computing. The gate is
now 8.8 seconds against a 7.8 second baseline.

That theorem carries one hypothesis, `depth_max < gen_max`, for the same
reason: 64 and 2147483647 are both pinned to the shipped source by the
capacity rows, but this representation cannot be asked to compare them. It is
written down as MODEL GAP 11 rather than smoothed over.

### What was left undone

`IR-FUN:ARGS-CK` is still pinned by its frozen text, and the reason is
recorded in the BINDING GAPS section of `formal/Common/Structure.v`. Its "this
block" arm cannot be driven by any builder row at all: the only writer of a
value row's block field is `IR-FUN:ADD-BLOCK-ARG`, which writes the same
`b BCNT` that `IR-FUN:END-BLOCK` later compares against, and nothing between
the two can move the block count. That makes it a second authority checked to
agree with itself, exactly the shape FINDING F2 already records for the stored
terminator ordinal.

What is worth driving and is NOT driven is the other arm: an argument window
that a minted operation result falls inside must be refused with
`E-IR-FUN-ARG`. Reaching it needs block arguments in the build sequence
vocabulary, which the block steps do not have — no row adds a block argument
today, so the whole of `ARGS-CK`'s loop is dead in the gate. That is a
separate piece of work: a new column on the block step for arguments added
before and after the block's operations, matching support in the Habu runner,
and a `vrow` list in the generated Rocq block runner so it can call the
model's `args_ck`. It should be dotted.

### The wider suite

`./bin/hb --load test/gate-stdlib.f` reports red phases rather than a fixed
set of six. The red set is UNCHANGED by this work, and that was measured
directly rather than inferred: the suite was run twice on the finished tree
and once on the unmodified parent change (checked out on its own, then put
back), and all three runs gave exactly the same two red phases,
`check-cli-boundary` and `compiler-ir-id`.

Neither is in a file this work touches, and both pass on their own —
`./bin/hb --load test/compiler/ir-id.f` and `./bin/hb --load tools/check-test.f`
are green when run directly. They fail only inside the parallel pool, and
`check-cli-boundary` takes 80 seconds there against 50 seconds standalone, so
they look like timing flakes under pool load. They are pre-existing and not
mine to fix here, but they are worth a dot of their own.

Also green on the final tree: `test/compiler/ir-storage-proof.f`,
`test/compiler/ir-storage-manifest.f`, `test/compiler/ir-structure-proof.f`,
`test/compiler/ir-structure-manifest.f`, `tools/suite-coverage-lint.f`
(164 suites, 0 findings), `tools/dot-dep-lint.f` (0 findings), and both diff
lints (`tools/typed-local-diff-lint.f`, `tools/package-diff-lint.f`) on the
`jj diff --git` artifact. The diff contains no `Admitted` and no `admit`, and
the Rocq build of `formal/Common/Storage.v` reports every published result
closed under the global context.
