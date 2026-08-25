---
title: "Typed storage sweep: 75 now, three view extensions"
status: open
priority: 2
issue-type: task
created-at: "2026-08-19T10:05:19.384331+02:00"
---

Phase 3 of 4fd12d60, class A (142 sites): 75 are plain VAR @/! on pre-hook variables - migrate the declaration to TYPED-VARIABLE, no extension needed. Three layout-buffer extensions cover the rest: ext-1 span-valued TYPED-VARIABLE (N-cell value read/written as a unit, ~20 sites: enum-decl.f:152-153 PEND!/@ pair, structure-decl.f:126-127, generated-declaration.f:443-446, hide.f BFR-*); ext-2 field-at-offset over an externally-owned base (data-base/DATAB/pointer var, ~16 sites: xref.f SEAL-NDICT@, env-base, debug, snap-lib); ext-3 indexed row view with per-field types INCLUDING quotation-typed cells (~19 sites: declaration-transaction.f ROW.COMMIT ( ptr a -- ptr [ n -- n ] ), xref.f WATCH-AT, gpt2-model M-SAVE/TAKE). 16 machine-code-emission sites are NOT this class - they stay sealed prims. Blocks the final deletion.

Claim: unassigned (RELEASED 2026-08-22: workspace gone, no live lane - the 2026-08-21 gc keyed on dot-id workspaces and missed the shared habu-thecut/habu-trusted names)

PREMISE FALSIFIED, RULING NEEDED (2026-08-20, storage-1's measured probes on
master 72049d7f through install --force and the quiet checker).

TYPED-VARIABLE CANNOT TAKE THESE CELLS. Measured admissibility of the real
definer (src/core/layout-buffer.f STORAGE-VALIDATE -> checker.f
CHECKER-STORAGE-INFO:3419), 20 stored types probed, 4 admitted:
  ADMITTED  a NEWTYPE/DEFTYPE arity-0 nominal, `ptr <that nominal>`,
            `[ n -- n ]`, `[ -- bool ]`
  REFUSED   n, a, u8, bool, i64, idx, len, fd, rc, count, xt,
            ptr a, ptr n, ptr u8, ptr ptr u8, ptr bool   (all rc 7121)
The gate admits nominal scalars, closed non-linear layout families, closed
typed pointers into those, and closed xt<effect> cells - and NOTHING
structural. test/typed-storage-test.f:154 has pinned `TYPED-VARIABLE BAD-N n`
as a REJECT since the definer landed. So "migrate the declaration to
TYPED-VARIABLE" is not executable for this class: 19 of the 24 post-hook sites
carry a `ptr u8` span and the rest carry `n`.

THE CHEAP CONVERSION IS A LIE, SO IT WAS NOT DONE. A plain `variable` already
certifies every structural accessor shape: measured verdicts on one raw cell -
`( n -- ) V !` -1, `( -- n ) V @` -1, `( ptr u8 -- ) V !` -1,
`( -- ptr u8 ) V @` -1, `( -- bool ) V @` -1, `( -- ptr ptr u8 ) V @` -1, and
only `( -- <nominal> ) V @` 0 (the introduction seal). Dropping TRUSTED: to `:`
therefore compiles - proved end to end: structure-decl.f PEND!/PEND@ flipped to
`:` and install --force went green - but it buys no guarantee, because the cell
type is an open variable that each call site instantiates afresh. It converts
an honest assertion into a vacuous certification. Same for ext-2's shape:
`( -- n ) data-base FRIEND-LATCH-CELL + @` certifies -1, and so do the `bool`
and `ptr u8` spellings of the same body.

THE MACHINERY DOES ENFORCE A CLOSED CELL TYPE when it has one:
`( n -- ) PTRVAR 0 ptr-field !` is REFUSED (verdict 0) while the `ptr a` and
`ptr u8` spellings certify. Pinning is the whole difference.

PRE-HOOK IS A HARDER STOP THAN THE CAVEAT EXPECTED. A pre-hook `variable` is
not mistyped, it is INVISIBLE: enum-decl.f:127 SUMV-N@ dropped to `:` dies
`E-UNDEFINED habu: in sumv-n@: undefined word 'SUMV-N'` /
`hook: non-certified definition: sumv-n@ at 'SUMV-N'` (SUMV-N is declared in
type-family.f, prefix row 11 of 16, before check-hook.f). 40 of the 64 sites
are blocked there - route 3 (64078d43), same recording gap as fab55650.

MEASURED CLASS (locator + per-site resolution of each target cell to one
declaration; the "142 / 75 mechanical" split does not reproduce):
  64 TRUSTED: bodies that are storage access and nothing else
     40 pre-hook target cell  -> BLOCKED on route 3
     24 post-hook target cell -> compile as `:` today, mean nothing until the
        cell type is pinned. src/habu/hide.f 6, src/habu/snap-lib.f 5,
        generated-declaration.f 4, enum-decl.f 2, structure-decl.f 2, test 5.
  1360 TRUSTED: sites tree-wide (187 empty-body casts, 1009 no storage).

THE THREE EXTENSIONS COLLAPSE INTO ONE. ext-3's quotation cell ALREADY SHIPS
(T-QUOT, checker.f:3429; `[ n -- n ]` admitted above). ext-1's "span-valued"
cell is two ordinary cells, a `ptr u8` and an `n`, and needs no N-cell
value form - only for those two types to be admissible. ext-2's engine-layout
offset already compiles and needs the same pinning, not a new view. So the one
sufficient change is: admit CLOSED STRUCTURAL cell types in
CHECKER-STORAGE-INFO (n, bool, u8, i64, the role tokens, and ptr chains
bottoming at them), width 1. One predicate in checker.f, no new definer, no new
keyword - and it makes all 24 post-hook sites sound rather than merely quiet.
That is a checker interface change, so it is NOT built until ruled on.

RULED AND LANDED (2026-08-20). CHECKER-STORAGE-INFO now admits a CLOSED
STRUCTURAL CELL: a non-linear one-cell T-CON, and a `ptr` chain bottoming at
any non-linear con. Measured admissibility, same 20 probes: 16 admitted where 4
were, and the four still refused are the ones that cannot be pinned at all -
`a` and `ptr a` (open var), `xt` (a bare atom, no effect to pin), and `u8`.

THE PIN THAT WAS FLIPPED, AND WHY IT WAS RIGHT TO FLIP IT. The reject list was
scoped, never a soundness claim. checker.f's own RAW discipline says so in as
many words: a raw cell "admits only a plain scalar representation and must
NEVER absorb a nominal atom, arity-0 family, layout, or nominal-bearing
pointer", and RAW-OK? "ADMITS a plain scalar/role con ... and REJECTS a
NOMINAL-FAMILY / LAYOUT value" - adding a CONCRETE con to the storage gate
cannot touch the nominal fence, and a structural accessor never needs the
LAYOUT-INTRO mint because structural cons were never fenced. The same note
names this work as its own follow-on: fencing role atoms out of raw storage
"needs that role/xt scratch migrated to typed cells first". Pin flipped in
test/typed-storage-test.f with that rationale written into the file, never
silently; the accept is section 10 and the negative control is section 9.

`u8` STAYS REFUSED, against the letter of the ruling and for a measured reason.
A stored `u8` mints `( -- ptr u8 )`, and cell `@` over a concrete `ptr u8` is a
checker error - `( ptr u8 -- n ) @` verdict 0, while `( ptr u8 -- u8 ) c@` and
`( ptr ptr u8 -- ptr u8 ) @` both certify. The cell would be unreadable by the
operator its own declaration implies while still costing a whole cell. Inside a
pointer chain the sub-cell cons stay admissible, which is the shape the span
cells actually hold. u16/u32/char excluded on the same measurement.

A THIRD BLOCKER CLASS, FOUND BY BUILDING IT. TYPED-VARIABLE is generative: it
evaluates the accessor through TDECL-EVAL-XT, armed at src/core/include.f:518,
prefix row 34. Every decl-machinery site sits BELOW that row - generated-
declaration.f (20), structure-decl.f (23), enum-decl.f (24) - so a conversion
there dies 7121 with the type long since admissible. Measured by converting all
eight and watching install --force fail; reverted. Separately, src/habu/hide.f
(6 sites) is appended into the stage2 source at tools/build-fixpoint.f:988 and
compiled by the OLD engine, so it can never use a capability the new engine
adds; snap-lib.f (5) is emitted the same way and is unprobed.
So the storage class is not one sweep but three bootstrap problems:
  40 recording gap        -> route 3 (64078d43), now a recorded blocker
   8 generative arming    -> needs INCLUDE-EVALUATE before prefix row 20
  11 stage2/old-engine    -> needs the capability in a SHIPPED engine first,
                             so these convert one full release after it lands
   5 reachable            -> 2 converted here, 3 are `create`-region views
                             (a different shape this change does not serve)
Zero reachable NON-TEST production sites exist: measured over every file that
is neither an early prefix row nor a stage source, requiring self-owned cells.

CONVERTED: test/compiler/native-rename-rows.f - ten cells pinned to `n`, all
EIGHT accessors (ZERO, GRAB1-5, KEEP, SAME) now ordinary `:`; and
test/checker-scan-index-suite.f SCX-NAME! with its span cells pinned
(`ptr u8` + `n`). Net -9 TRUSTED: sites, 1360 -> 1351.
