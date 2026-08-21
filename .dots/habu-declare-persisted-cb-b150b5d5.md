---
title: Declare persisted callback-table xt cells
status: open
priority: 1
issue-type: task
created-at: "\"2026-07-30T14:59:41.862546+02:00\""
---

PRIORITY 1 and now THE remaining blocker for a restored snapshot image that can load library source; it is the residual named at the end of dot habu-declare-persisted-producer-76fbce09, and it is the same relocation class with a different owner. With both lowering-certificate producers converted to declared defers, a restored image compiles and runs a definition, installs and dispatches a defer, and passes tools/build-fixpoint-test.f, but it still dies rc 134 the moment it evaluates a GENERATED DECLARATION. Minimal reproducer on a plain image built by 'bin/hb --load tools/build-fixpoint-refresh.f -- snap': feed it the single line 'sumtype option2<a> = none2 | some2 a ;sumtype'. That is also why 'require lib/string.f' and therefore test/owner-wid-internal.f are still red. EVIDENCE, measured 2026-07-30. The faulting instruction is 'blr x9' at the end of the engine's catch frame setup (the caller stores CATCH-FRAME-MAGIC 0xCA7CF4A3E00D from src/habu/layout.f, and lldb disassembly at the return address shows the store-frame/store-magic/blr sequence), so the crash is a catch of an execution token that came out of a cell. The token is identical in three consecutive runs of the same image (0x1014d0aa4) while the live region base differs in each (0x103760000, 0x103a00000, 0x1037b0000), and the same 64-bit value appears once in the image FILE as a contiguous little-endian cell, so it is persisted, not computed, and it is the writing run's absolute region address. The owner is the declaration-transaction participant table. src/core/generated-declaration.f 'create PARTICIPANT-BOOT ... allot' and 'create STATE ...' are DP-heap arrays; src/core/declaration-transaction.f stores five quotations per participant row through SNAPSHOT!/PREPARE!/COMMIT!/ROLLBACK!/RELEASE! and two more in the state record through ST.ALLOCATOR and ST.DIAGNOSTIC, and CALL-PARTICIPANT then runs one with 'expected callback catch' while ALLOCATOR@/RELEASE@ run theirs with 'execute'. Five participants enroll at boot (checker frame, DECL-EVENT, constructor generation, native dictionary, protection), so about 27 persisted cells hold region addresses and NONE of them is declared to the snapshot address-cell table (src/habu/layout.f SNAP-RELOC:XTCELL-*), because 'defer' and 'is' are still the only two declaration points and neither can name a cell that is computed at run time from a table base and a row index. This is exactly the deeper hole the producer dot named: the declared-kind design is only as complete as the set of ways a cell can come to hold a token, and an ARRAY of dispatch cells filled by a store through a computed address is outside it. A defer per slot is not the fix (the table grows and its rows are chosen by index at run time); the missing capability is a way for the code that DECIDES a cell will hold an execution token to say so about a cell whose address it computes, with the same fail-closed shape 'defer'/'is' already have, plus a checker rule that makes an undeclared store of a quotation into a raw cell rejectable rather than merely discouraged. Acceptance: the one-line sumtype reproducer runs on a restored image and exits 0; 'require lib/string.f' loads on a restored image; test/owner-wid-internal.f goes green through its suite path; test/snapshot-xt-cell-decl.f gains rows for a table-shaped declaration (a declared row relocated, an undeclared lookalike row untouched) and a negative regression proves an undeclared table cell of this shape is rejected or relocated.

Claim: agent=cbdeclare workspace=.jj-ws/habu-declare-persisted-cb-b150b5d5 (RELEASED 2026-08-21: workspace gone, no live lane - gc)

MEASURED 2026-07-30 (agent=cbdeclare, commit "Declare persisted callback table
cells"). The owner this dot names is right, the crash is gone and proven gone,
and the missing capability is now a primitive rather than a convention.

WHAT THE CHANGE DOES. The engine gains one primitive, `xt!` ( q ptr q -- ):
store an execution token into a persisted cell and declare that cell to the
address-cell table in the same step. Its body is src/habu/habu2.f
SNAP-RELOC:BXTSTORE, which is habu1.f BSTORE's guarded store followed by a call
to LMARK, the declarer that `defer` and `is` already call from their compile
handlers. It lives in package SNAP-RELOC, next to the declarer, and is
registered as a framed deref prim with minimum input depth 2, exactly like `!`.
src/core/declaration-transaction.f then writes its seven callback cells with
`xt!` instead of `!`: the five per participant row (SNAPSHOT! PREPARE! COMMIT!
ROLLBACK! RELEASE!) and the two in the state record (the allocator and the
diagnostic). Nothing else changed in that file, and the two bookkeeping cells in
the same rows -- identity and order -- keep their ordinary `!`, because they
hold ordinary integers.

WHY THE STORE WORD AND NOT THE TABLE'S CREATION. Both were open, and the dot
asked for the choice to be justified from the code. The store word wins on two
counts. The table is not fixed geometry: GROW-TABLE asks its allocator for a
bigger block and MOVE-ROW copies live rows into new cells while participants are
being registered, so a creation-time walk would have to be repeated at every
grow -- a second decision site to forget. And declaring at creation would
declare cells that may never hold a token, which is a guess about the future
rather than a fact about a store. The store word is the code that decides the
cell will hold a token, so it is where the declaration belongs. LMARK turned out
to be callable at that phase without any change: the participants enroll while
ordinary checked Habu is running, long after the engine is up, and the routine
already saves and restores every register it touches.

WHY ONE PRIMITIVE AND NOT TWO WORDS. A bare "declare this address" word plus an
ordinary `!` would let the two halves be written apart and drift. `xt!` cannot
be half-applied.

MEASURED, every number with `bin/hb --load tools/build-fixpoint-refresh.f --
install --force` rerun on this workspace first, and each measurement in its own
private HB_TMP root.

  A generated declaration through a restored image, on an image built by
  `-- snap`: an `ENUM option2 1` with a `none2` variant and a `some2` variant
  carrying a field, then a checked definition returning `option2<n>`, then an
  ordinary definition run for output.
     before: exit 134, SIGSEGV, register dump.
     after:  exit 0, output identical to the same script through bin/hb.
  `require lib/string.f` through a restored image:
     before: exit 134.   after: exit 0.
  50 consecutive boots of that image, each running the whole declaration
  script: 50 exit 0, 0 failures, output byte-identical every time.
  test/owner-wid-internal.f through its own load path:
     before: 3 failures, exit 1.   after: exit 0, "owner-wid-internal-test: ok".
  tools/build-fixpoint-test.f: exit 0, "build-fixpoint-test: ok".
  test/snapshot-xt-cell-decl.f: exit 0, with eight new rows (below).
  test/generated-declaration-transaction-suite.f, test/declaration-release-
  inventory.f, test/owner-wid-guard.f, test/compiler/reloc-proof.f,
  test/compiler/reloc-manifest.f: all exit 0.
  Declared address cells, the direct measure of this class: a cold bin/hb goes
  62 -> 89 and a dev snapshot image 128 -> 182. The +27 in bin/hb is exactly the
  five boot participants' five callbacks each plus the state record's two.
  Capacity is 4096, so the table is at 2.2 percent.
  Engine fixpoint rebuild green; self-check census 0 uncheckable, 0 rejected,
  certified 4257 -> 4258. bin/hb is 148855 bytes, unchanged, so the committed
  size ratchet row in test/gate-build-size.f needs no bump.
  package-diff-lint exit 0, typed-local-diff-lint exit 0, error-code-lint
  0 finding(s), trust-lint 0 finding(s), dot-dep-lint 0 finding(s).

THE ONE-LINE REPRODUCER IN THIS DOT'S TEXT IS NOT VALID SOURCE. The line
`sumtype option2<a> = none2 | some2 a ;sumtype` is rejected with "name must be a
lowercase family tail", exit 67, by bin/hb and by a restored image ALIKE, before
and after this change. It never reached the declaration transaction, so it was
never this class's reproducer; the ENUM script above is, and it is the shape
lib/adt/option.f actually uses.

NEW TEST ROWS. test/snapshot-xt-cell-decl.f gains a second half that drives the
REAL coordinator -- src/core/declaration-transaction.f, the one the five boot
participants enroll through -- over a three-row table the test owns, and watches
the same engine table the rest of the file watches. Four count rows: INIT
declares the state record's two callback cells, a first REGISTER declares its
row's five, a second declares its own five, and a third whose order sorts ahead
of both declares only five more, because OPEN-SLOT's MOVE-ROW re-stores into
rows that are already declared and the table refuses duplicates. Then a
membership row (all seventeen callback cells are listed) and two negatives: the
identity and order cells of those same rows are NOT listed, and a lookalike
table of identical geometry filled with a real execution token through an
ordinary `!` has none of its cells listed while really holding that token. The
field offsets are read out of the coordinator's own layout constants rather than
written down again.

test/compiler/reloc-schema.f gains two closure rows. The gate froze the caller
set of SNAP-RELOC:MARK-CELL, which was the only way into the declarer while
`defer` and `is` were the only declaring sites; a run-time caller reaches LMARK
directly and was invisible to it. Rows 12 and 13 freeze the two names the
declarer goes by in that file -- bare inside package SNAP-RELOC, qualified
outside -- so a new caller shows up whichever side of the package boundary it is
written on.

FALSIFICATION (measured, every mutation applied and reverted; src/habu/habu2.f
and src/core/declaration-transaction.f are byte-identical to their pre-mutation
state in the final tree).
  1. Revert ONE store word: SNAPSHOT! back to plain `!`, everything else left
     alone. Rebuild, re-snap: declared cells 89 -> 84 (the five participants'
     snapshot callbacks), and the reproducer dies rc 134 again with the same
     signature -- CATCH-FRAME-MAGIC 0xCA7CF4A3E00D in the register dump and a
     program counter of 0x1062e0aa4 with this run's region at 0x1050a0000.
     `require lib/string.f` dies 134 again too. Restore, rebuild: exit 0.
     So a single one of the seven declarations is load-bearing.
  2. `xt!` stops declaring (delete the LMARK call from BXTSTORE, leaving the
     store). Rebuild: exactly the five new POSITIVE rows of
     test/snapshot-xt-cell-decl.f go red and the fourteen pre-existing rows stay
     green; the reproducer dies rc 134. The two new negatives stay green, which
     is correct -- they assert absence.
  3. The bookkeeping cell is declared too (ID! uses `xt!`). Rebuild: the new
     negative row "the identity and order cells of those rows are not" goes red,
     0 -> 3, along with the count deltas. So that negative has content.
  4. BXTSTORE stops calling the declarer, source only: reloc-proof's new closure
     row 12 goes red and nothing else.

BEST LONG-TERM FIX OR A PATCH? Long-term, re-derived from the code. The
invariant is that a persisted cell is relocatable only if the code that decided
its kind said so, never a guess about its contents. The previous two declaration
points were `defer` (allocation) and `is` (store), both compile-time, and the
hole was not that they were wrong but that they cannot NAME a cell whose address
is computed at run time. This adds a third declaration point of exactly the same
shape at exactly the same place in the causal chain -- the store -- and no value
test, no address range, no sentinel, no scan. The proof that membership still
comes from a decision and not from contents is the new lookalike-table negative:
twenty-one cells holding a genuine live execution token, in a table with the
same geometry as the declared one, all of them out of the table.

The places this could have been a patch, and what was done instead. A `defer`
per slot was refused for the reason the dot gives, and so was declaring at the
table's creation, on the grow and MOVE-ROW evidence above. The declaration was
NOT made a separate word that a caller must remember to pair with a store. And
the reloc parity gate was extended rather than left green-by-accident: it stayed
green through this change only because the token it watched did not move, which
is precisely the kind of silence a gate is supposed to break.

HONEST GAPS, named not absorbed.
  - The prim row for `xt!` is `!`'s row, PE-A PE-IN PE-PTR-A PE-IN. That holds
    the stored value and the pointee to the same type, so a quotation-typed cell
    can only be written with a quotation of exactly its own effect. What it
    cannot say is that the pointee MUST be an execution token: the checker has
    only TVK-ANY and TVK-RAW, no quotation-kinded type variable, so `xt!` into a
    plain integer cell type-checks and would have the loader shift an ordinary
    integer. The same missing kind is why a plain `!` of a quotation into a
    persisted cell is still not a reject -- which is how this bug lived for
    months in a file whose pointee type is literally `ptr [ n -- n ]`. Dotted
    with its acceptance as habu-add-a-quotation-1610f30c. This dot's own
    acceptance asked for "rejected OR relocated"; this change delivers
    relocated, and the reject needs that capability first.
  - A coordinator that grows its table abandons the old block to the bump
    allocator, and the cells in it stay declared for the life of the image.
    Nothing is corrupted, because that memory is never handed out again, but the
    rows are never recovered against a fixed capacity of 4096. No ordinary boot
    grows the participant table at all and the table sits at 89 rows, and
    overflow is a loud exit (XTCELL-RC 96), not silence. Dotted as
    habu-withdraw-declared-cells-8c3c5df4.
  - bootstrap/cg/forth.fs registers `xt!` as a plain store, because that
    recovery seed has no address-cell table and carries its own snapshot format
    version; each build path relocates its own format. It is the source
    src/core/declaration-transaction.f needs to compile while the seed builds
    the real bin/hb, and the native refresh takes over immediately after.
  - Introducing the primitive needs a two-stage build from an engine that
    predates it: the emitter and the prim row first, then the callers. The
    committed tree is self-consistent in both directions -- the gforth seed has
    the mirror, and a bin/hb built from this tree has the real one.
  - No full gate-stdlib and no test/run.f were run, per this lane's
    instructions.
  - test/owner-wid-internal.f is green in every private HB_TMP root it was given
    (eight passes across six roots, three of them freshly created for the
    purpose), and it fails reproducibly in a root where the dev snapshot engine
    hb-new has been RUN: the live-image leg then produces nothing and is killed
    by the harness's 120-second timeout. That trigger is not something any gate
    creates, and the leg could not hang before this change because it died rc
    134 instead. Recipe, controls and what is ruled out are in
    habu-owner-wid-live-534dc4af.
