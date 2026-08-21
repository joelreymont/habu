---
title: Freeze compiler IR builders
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-07-26T22:54:58.885952+02:00\\\"\""
closed-at: "2026-08-15T14:07:28.830131+02:00"
close-reason: "Closed (vintage audit 2026-08-15, re-executed after the pool incident): freeze builders (all-or-nothing FREEZE, verifier arm build.f:1332). Production-consumed by the native chain; suites dual-registered, green through the real entry."
---

Full context: design sections 6.4-6.5 require unique builders, abort, validation, and atomic freeze. Implement NEW-BUILDER, append APIs, ABORT, FREEZE result ownership, committed ceilings, and removal of mutation authority. Acceptance: every freeze arm returns context; refusal publishes nothing; abort releases all provisional storage; use-after-freeze/double-freeze/frozen mutation reject. Dependency: function/block control tables.

Claim: agent=irfreeze workspace=.jj-ws/habu-freeze-compiler-ir-6f706100

## Result (irfreeze, measured)

What landed. `src/compiler/ir/build.f` opens package IR-BUILD and owns the
builder and freeze lifecycle. A builder is created by `NEW-BUILDER` against a
live context and a named dialect; it creates and privately holds the fifteen
arenas a module is made of (symbol pool and rows, type pool and rows,
attribute pool and rows, source registry, schema pool and rows, operation cell
pool, value rows, operation rows, function attribute pool, function rows,
block rows). Because those handles never leave the package while the builder
is live, the builder handle is the only way to reach them, which is what makes
it the single mutation authority. Every append word delegates to the module
that owns the table it writes: IR-SYM, IR-TYPE, IR-ATTR, IR-SOURCE, IR-SCHEMA,
IR-OP, IR-FUN. No table logic is repeated here.

Ownership without a forging cast. The fifteen handles and the module key and
identity are kept in typed storage (`TYPED-BUFFER` over `IR-ARENA:arena`,
`IR-ARENA:view`, `IR-ID:ir-module-key`, `IR-ID:ir-module-id`), so IR-BUILD
never re-mints a raw cell into another package's sealed family. The context
handle is deliberately not stored, because IR-CTX's rule is that no context
handle survives its `WITH-CONTEXT` body: every mutating word takes the context
from its caller and checks it against the owner serial the slot recorded,
which is also what rejects a builder used from another compilation.

Freeze is all or nothing. `FREEZE` runs every refusal arm before the first
arena is frozen: the builder must be live, it must hold no open function,
block or operation, every table must hold no more records than the ceiling
this builder committed to, and all fifteen arenas must still be live. Only
then are they frozen and the module handle minted. A refused freeze therefore
changes nothing, which the test measures directly against the context's module
count and scratch cursor, the table counts, and the builder's own liveness -
and then finishes the record and freezes successfully with the same builder.

Abort. `ABORT` gives back every stage this builder holds, retires all fifteen
arenas at once, and marks the slot aborted so a later use of the handle is
named rather than merely unknown.

Stage ownership. IR-OP and IR-FUN keep one open record each per process, not
per store, so IR-BUILD records which builder holds each of the three stages.
A claim whose holder is no longer a live builder is reclaimed on the next
attempt, and the authority's orphaned record is abandoned with it; without
that, one construction abandoned by a throw would keep every later builder out
of that stage.

Error codes: the reserved block -8060..-8079 is now claimed in `lib/errors.f`
as E-IR-BUILD-STALE, -FROZEN, -ABORTED, -OWNER, -OPEN, -STAGE, -PLAN,
-CEILING, -SLOTS, -SERIALS and -STATE (eleven of twenty). A twelfth code for
"module handle where a builder belongs" was written and then removed: a module
handle is minted only by FREEZE and a frozen slot never changes state again,
so no checked program can reach it, and a dead code is worse than none.

Measured gate results, all on this workspace over parent 215052d1:

- `./bin/hb --load test/compiler/ir-build.f` - exit 0, `test: ok`.
  Before the module existed the same path failed with exit 74,
  `include: open failed`.
- `./bin/hb --load tools/error-code-lint.f` - exit 0,
  `1324 file(s), 855 claim(s), 39 reservation(s), 0 finding(s)`.
- `./bin/hb --load tools/suite-coverage-lint.f` - exit 0,
  `165 suite(s), 0 finding(s)` (164 before; the new suite is registered in
  `test/gate-stdlib-cases.f` and routed in `test/gate-stdlib-inline-lib.f`).
- `./bin/hb --load tools/package-diff-lint.f -- <jj diff --git>` - exit 0, no
  findings.
- `./bin/hb --load tools/typed-local-diff-lint.f -- <same artifact>` - exit 0,
  no findings.
- `./bin/hb --load test/gate-stdlib.f` - baseline on this tree before the
  change: `red phases: 6` (engine-error-package, pre-trust-defer,
  aot-wid-restore, owner-wid-internal, stdlib-process-fixtures,
  build-fixpoint-fixtures). After the change, twice: `red phases: 7` - the same
  six plus `hb-build-fixtures kind=TIMEOUT-UNDER-LOAD code=0 ... ran=120128ms`.
  That seventh red is machine contention, not this change: all four members of
  that suite pass individually (`tools/hb-build-test.f`,
  `lib/build-cache-test.f`, `lib/codesign-test.f`,
  `tools/hb-build-direct-lints-test.f`, each exit 0), and
  `tools/hb-build-test.f` alone takes 159 seconds of wall time against a
  120-second budget at load average 23 on this machine. The suite loads
  neither new file.

Mutation evidence that the tests do work. Removing the open-record check from
FREEZE turns the suite red (uncaught -8061). Removing the arena release from
ABORT turns it red (uncaught -6657, the arena registry running out). Wiring one
published view to the wrong table turns it red (uncaught -8020). Restored and
green after each.

Known limits, recorded rather than hidden. Fifteen arenas per module against
IR-ARENA's sixty-four registry slots means at most four modules can be live at
one time in a process; the builder registry holds sixteen slots per context
lifetime. Both are named refusals (E-IR-ARENA-SLOTS, E-IR-BUILD-SLOTS) and both
are reclaimed at context teardown. Raising either is IR-ARENA's or this file's
own capacity decision and was not taken here. The structural freeze verification
of design section 6.5 (dominance, derived predecessor and successor tables,
attribute canonicalisation, span validity) belongs to the neighbouring dot
habu-verify-frozen-compiler-224d78ad and is not duplicated here; when it lands
it becomes one more refusal arm in front of the arena freezing.

## Interface change recorded after the fact (irverify, 2026-07-30)

Two things this dot's record described have since changed, in the lane for
habu-verify-frozen-compiler-224d78ad. Both were reviewed and approved before
implementation; this note keeps the freeze dot's record matching what ships.

**`FREEZE` takes the compilation context.** It is now
`FREEZE ( IR-CTX:ctx IR-BUILD:builder -- IR-BUILD:module )` and passes the same
`USE` gate every append word passes, rather than only checking that the builder
is live. Two independent reasons. Section 6.5's freeze validation has to consult
the bound target contract and needs an allocator for the tables it derives, and
both live in the context, which this package deliberately does not store.
Separately, publication was the one state change IR-BUILD made without proving
the caller owns the compilation, so a builder handle that escaped into another
compilation could have published a module its context never agreed to. A new
fixture in `test/compiler/ir-build.f` proves that a live but foreign context is
refused with `E-IR-BUILD-OWNER`. All 21 call sites in that file were migrated,
including the checker-sealing fixture that spells the effect literally.
`ABORT` keeps its narrower signature: it publishes nothing and retiring an arena
needs no allocator.

**A module is seventeen tables, not fifteen.** `NEW-BUILDER` also creates the
derived block-edge table the freeze verifier fills - a predecessor pool and one
row per block - with ceilings read off the same committed plan, so verification
allocates nothing of its own at freeze time. They are published as
`FEDGE-POOL` and `FEDGE-ROWS`. The `TTABLES` pin and the fifteen-table prose in
the test were updated with it. The capacity consequence is recorded honestly:
seventeen arenas per module against IR-ARENA's sixty-four registry slots means
three modules can be live at one time rather than four, both limits are named
refusals; raising the arena registry slot count is a one-constant capacity
change IR-ARENA makes when a real consumer needs more than three live modules
(no dot - no consumer needs it yet).

**The structural verifier has landed.** This dot's closing note said the section
6.5 verification "becomes one more refusal arm in front of the arena freezing
when it lands". It has: `FREEZE` calls one public `IR-VERIFY:VERIFY` after the
ceiling and liveness arms and before `TABLES-FREEZE`, and `build.f` holds no
verifier logic of its own.
