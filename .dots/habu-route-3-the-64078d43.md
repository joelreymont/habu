---
title: "Route 3: the type foundation loads post-hook, checked"
status: active
priority: 2
issue-type: task
created-at: "2026-08-19T10:53:19.909085+02:00"
blocks:
  - habu-typed-storage-sweep-b2cd1a61
---

The zero-trust route for the 485 recording-gap TRUSTED: sites (fab55650's blocked set, epic 4fd12d60): src/habu/habu2.f:861-890 loads type-schema.f, type-family.f, sumtype.f and checker.f BEFORE src/core/check-hook.f installs the hook, so their : definitions record no signature - measured 2026-08-19 by trusted-1 through the real prefix build (ED-PROBE rc 70 evidence on the leaf of fab55650). Move the type foundation post-hook so its signatures are DERIVED AND CHECKED - the bootstrapping knot is the checker checking its own foundations, which is the same territory as habu-seal-the-checker-5314c0ab (package ownership of checker.f) and the dissolved umbrella habu-tfam-2b-sealed-1b77662c (re-derive before dispatch). Rejected alternatives, with reasons on fab55650's leaf: owner-side declared-signature recording (trust that route 3 would delete), mass PRIM axioms (contradicts the epic by its own text). This blocks fab55650's remaining 485 sites and therefore feac682b (the reader deletion).

SCOUTED VERDICT (2026-08-20, full map in the scout report; re-measured):
1. THE KNOT IS ONE EDGE, NOT FOUR FILES: check-hook.f's closure is checker.f
   alone (0 hits into the 825 type-file names). The only real obstruction is
   render.f -> type-family (13 token sites, 5 load-bearing in REND-SIG's path)
   - render.f must stay pre-hook because it installs RECXT, the ONLY
   inferred-effect row producer, whose default silently discards signatures.
   Cut with the existing checker defer wall (checker.f:465-482/:896-907,
   bound at type-family.f:3218-3239) - proven in-tree instrument.
2. SCOPE: move type-schema.f, type-family.f, sumtype.f, layout-buffer.f,
   layout-valid.f AS ONE BLOCK to immediately after the hook row (zero-length
   window; the last two have zero pre-hook consumers and cost nothing).
   checker.f CAN NEVER MOVE: its 129 forwarder sites (70 distinct words) are
   conceded to PPRIM: axioms - the tree's own stated preference
   (prefix-rewind.f:41-43, LESSONS.md:204-209). Route 3 unblocks 352 sites,
   not 485; the ruling says so.
3. REPLAY-AT-HOOK IS STRICTLY DOMINATED: it still requires the 662 definitions
   to typecheck and adds either route-1's declared-text carrier (rejected) or
   a second CHECK pass over the same bytes (second authority).
4. PRECEDENT PRICED: structures.f moved across the hook 2026-07-15
   (ksxvzllqmnks) - 14 files, 11 ordered manifests, the exact churn list is in
   the scout report.
5. HARD GATE BEFORE ANY MANIFEST EDIT: run tools/check-core.f over the three
   files and COUNT THE REJECTS. 662 definitions have never met the checker.
   Encouraging: near-zero raw-pointer surface in schema/family. Discouraging:
   their post-hook peers run 35/44 and 39/55 TRUSTED:-to-definition ratios.
   If the owner-side bill approaches that, route 3 nets far less than the
   epic assumes - re-open the ruling on that number.

Claim: agent=route3-3 workspace=.jj-ws/habu-trusted (TAKEOVER 2026-08-21 from
route3-2, which stood down; the three seals that blocked it have landed)

HARD GATE ANSWERED (2026-08-20, lane route3-1). ALL 662 DEFINITIONS
TYPECHECK. Measured through the real driver, tools/check.f -> check-core.f
(hook-sites row 6), staged in the prefix order route 3 proposes, each file
carrying the preceding ones as cross-file support:
   bin/hb --load tools/check.f -- --json-errors --all-errors --source-list \
       <preamble> ./src/core/type-schema.f ./src/core/type-family.f \
       ./src/core/sumtype.f
Counts, per class (a) clean / (b) honest type error / (c) capability gap /
(d) probe artifact:
   type-schema.f    62 defs   62 clean   0 type errors  0 gaps
   type-family.f   411 defs  411 clean   0 type errors  0 gaps
   sumtype.f       189 defs  189 clean   0 type errors  0 gaps
There is not one honest type error and not one capability gap in the block.
Every reject the probe ever printed was class (d) and each was traced to its
cause, not waved away:
 - The driver SKIPS a canonical prefix path at expansion, preverify,
   all-errors and child-run alike (CHK-DEP-PRELOAD? -> REQUIRE-KNOWN?, an
   exact byte compare against the engine-provided registry), so
   `check.f --source-list src/core/type-schema.f` exits 0 having checked
   NOTHING. Spelling the same file `./src/core/...` restores it as real
   input. This generalises LESSONS.md:784 - the failure is silence, not the
   E-UNDEFINED that lesson predicts.
 - sumtype.f's own `PRIM: NEWTYPE/SUMTYPE/PRODUCT PRIM;` axioms are already
   in the parent's checker registry, so re-checking the file dies
   `checker: duplicate definition` at line 1819. The probe renames those
   three heads and their three axiom rows, nothing else; mutating each of
   the three bodies is still caught, so all three are really checked.
 - src/habu/xref.f:673 `undefine TFAM-PKG-XT` retires, far downstream of the
   block, the defer that type-family.f:3220 binds. Restoring exactly
   checker.f:469's declaration in the probe makes TFAM-HOOK-INSTALL certify.
   That was the LAST surviving reject, and it was ours, not the file's.
   Cross-checked: TFAM-PKG-XT is the ONLY prefix-undefined name the block
   references.
OWNER-SIDE BILL: 86 distinct rows - 83 on checker.f, 1 each on cell.f,
util.f and render.f (TDECL-DIAG, the one render edge the scout named).
64 rows copy a signature the owner already declares, 13 are the effect the
compiler itself publishes for `variable`/`create`, and 9 are signatures that
DO NOT EXIST in the tree today and have to be written (TAG, PAY, MK-CON,
MK-VAR, MK-ROW, MK-PARAM, MK-QUOT, CHECKER-STEP, USIGS-COPY, TKF, CON-OF -
all headerless checker.f definitions).
THE RULING DOES NOT REOPEN - THE GATE ARGUES THE OTHER WAY. 86/662 = 0.13,
against the siblings' 35/44 = 0.80 and 39/55 = 0.71, six times lighter. And
those sibling bills are mostly THIS defect: 27 of structure-decl.f's 35
TRUSTED: rows and 31 of enum-decl.f's 39 are one-token forwarders into
type-family.f / type-schema.f (FAM-DECL -> TFAM-DECL, SCH-ROOT@ ->
SCHEMA-ROOT@, ...). Route 3 pays 86 rows on checker.f and deletes 58 of them
outright in its two direct consumers, on top of the 352 sites the epic
counts. Tractable, and the number the scout feared is evidence FOR the move.
ONE DESIGN NOTE FOR WHOEVER TAKES THE MANIFEST EDIT: 13 of the 86 rows hand
out `-- ptr a` raw mutable cells of checker.f's private state (OK, FAILSET,
MULTI-ERR-N, SI, SL, TKFU, HIDX-GEN, PARAM-SCR-N, FIELD-FAM, CTOR-PEND-*),
because the block pokes those latches directly. A checker-effects.f that
exports raw storage handles is the shape master 0cc8d823 is about; decide
whether those 13 become accessor words before the rows are written.
Falsified, not asserted: a planted `( n -- n ) drop ;` at the FIRST and the
LAST top-level point of each of the three files is reported (so the pass
covers the whole file - this is how the NEWTYPE collision was caught);
dropping any sampled owner row brings back its exact E-UNDEFINED (16/16);
`is` against a TRUST row rejects while `is` against a real `defer` of the
same effect certifies; and a wrong hand-written row is not free - reading
`( -- t )` out of MK-QUOT's trailing comment forged a spurious reject until
the signature was read off the body instead.
ONE HONEST LIMIT ON THE NUMBER: the 64 copied rows repeat headers that are
themselves pre-hook and have never been verified, so if a header lies the
measurement inherits the lie. That is not a probe defect - it is the same
text a TRUSTED: row would carry, so the bill is exact for what route 3 would
actually write. The harness lives in the lane scratch (route3-gate3.py,
route3-falsify.py, route3-symidx.py); no production file was touched.

BLOCKED-ON habu-tfam-2b-sealed-1b77662c (2026-08-20, lane route3-2, ruled by
the orchestrator). Route 3 was BUILT, GATED AND STOPPED. The whole derivation
is banked in the lane at commit 1b73ba47 ("codegen: check the type foundation
past the hook", .jj-ws/habu-trusted, 31 files, +567/-329, NOT pushed, NOT
merged); most of it survives the reordering and it is the record the next lane
starts from rather than re-deriving. Nothing below is an estimate: every number
came off the real prefix build.

WHAT IT DELIVERED, so the size of the prize is on the record. src/'s TRUSTED:
census goes 218 -> 123: NINETY-FIVE rows deleted. Per file: enum-decl.f 39->9,
structure-decl.f 35->8, generated-declaration.f 23->13, structure-make.f 8->2,
decl-event.f 3->0, plus one each in generated-declaration-dictionary.f and
generated-declaration-protection.f. 74 of those were one-token forwarders into
the moved block; the other 21 were forwarders into checker.f words the new rows
answer (CON-OF, CC-N/CC-BOOL/CC-R, CHECKER-PACKAGE-PUBLIC, MULTI-ERR?,
CTOR-PEND-CLEAR, ARENA-BYTES-GROW, REG-GROW1, TYPE-NAME:CONTROL?,
TYPE-NAME:VARIANT-REQUIRE, SUMV-N@, MULTI-COUNT+). The block builds a
byte-identical fixpoint twice (872d9292...), maki/test.f rc 0, typed-local
diff lint 0, error-code-lint 0, dot lint 0, bootstrap-mirror-lint 0, and
boot-pin, diagnose-hb, using, type-family, type-decl, enum-decl,
structure-decl, layout-valid and lower-cert suites all rc 0.

THE STOP - FINDING 1, MEASURED. Recording a signature IS publishing an API.
src/core/internal-mark.f welds the two together by design: "the executable
top-level name universe equals the checker's". So making the registry checked
publishes its 653 GLOBAL definitions (type-schema.f 60, type-family.f 359,
sumtype.f 202, layout-buffer.f 32; layout-valid.f 0 - it is fully packaged)
as top-level-executable, checked-callable user API. This four-line user
program runs to exit 0 on the candidate engine:

    TFAM-N@ . cr        \ 119
    TFAM-RESET          \ wipes the family registry from user source
    TFAM-N@ . cr        \ 0
    : LEAK-PF ( n n ptr u8 n -- n bool ) PF-FIND ;

The tree's own gate says that must be refused: test/internal-word-gate.f
OPENER-CASES asserts `: IWG-PF-RAW ( n n ptr u8 n -- n bool ) PF-FIND ;` is
rejected at 'PF-FIND' ("raw implementation names are not checked/public", dot
habu-protect-type-field-04d91409), and that is the assertion the candidate
breaks. The same mechanism re-published CT-LIVE? (reversing
habu-internalize-field-liveness for its one historical consumer,
type-family.f PF-NODE-KIND?) and T-RES. RULING: route 3 lands AFTER the
registry is sealed. Trading 95 package-private unchecked declarations for 653
globally-callable registry internals is the fix-review gate's definition of
worse.

THE CONTAINMENT IS PACKAGING, AND THE LANE PROVED IT ON A WORKING EXAMPLE.
src/core/lower-cert-effects.f publishes 16 rows inside `package LOWER-CERT`,
before `public`. Bare MAGIC-V, BUF-N@, HEADER-N and LOWER-CERT:HEADER-N all
answer E-UNDEFINED at top level: the rows are real to the package's own
checked half and invisible to everyone else, with no visibility change at all.
That file is the worked shape the sealing lane should copy.

FINDING 2: A RECORDED ROW OUTLIVES `undefine`. type-family.f:1337's
`defer TDECL-FIELD-CLEANUP-XT ( n -- )` records a row once the file is
post-hook; generated-declaration-protection.f:196 retires the dictionary entry
but NOT the row, so CHECKER-RESOLVES? answers true for a retired seam
(test/type-field-owner-suite.f assert 188). That suite's own header explains
what is lost: the seam is a `defer` rather than a PRIM precisely so `undefine`
can retire it, and a post-hook row gives it the primitive's un-retirable
property. Falsified properly: a child load naming it still exits 70
E-UNDEFINED, so the dictionary boundary holds and no unsound acceptance was
demonstrated - this is the loss of the middle boundary of three, not a hole.
Full sweep of every `undefine` in src/ over moved names: TDECL-FIELD-CLEANUP-XT
and TDECL-FIELD-RELEASE-XT (generated-declaration-protection.f:196-197),
FULL-PRODUCE and FULL-PRODUCE-INSTALL (lower-cert-seal.f:22-23); plus three
names the lane's own rows published that are sealed away later - LBUF-PEND!
and LBUF-PEND-CLEAR (layout-buffer-seal.f:4-5) and FULL-INSTALL
(lower-cert-seal.f:21). This generalises route3-1's artifact 3 (xref.f:673).

FINDING 3: THE SCOUT'S "COSTS NOTHING" WAS FALSE, AND THE PACKAGE LINT BLOCKS
THREE FILES. layout-buffer.f and layout-valid.f were admitted to the block on
the ground that they have zero pre-hook consumers. True, and irrelevant: their
own BILL was never measured. layout-valid.f reaches the registry at 19 names /
29 sites so it CANNOT stay pre-hook, and moving it splits `package LOWER-CERT`
across the hook (lower-cert-base.f must stay pre-hook - it arms the
certificate dispatcher the checker calls at every publish). Paid with the
16-row lower-cert-effects.f above. Separately, tools/package-diff-lint.f
reports six E-PACKAGE-OWNERSHIP findings that no amount of care avoids: the
four new label variables in habu2.f, BPT-PFX-ROW# in test/boot-pin-test.f, and
BP-EACH in tools/boot-pin.f. CONTROL RUN, so this is attributed and not
assumed: an inert trailing comment on an UNTOUCHED global line in habu2.f does
NOT fire the lint, so these are genuine new/changed globals in unpackaged
files, not the blanket line-freeze LESSONS.md describes. Adding any prefix
source needs a packaging cascade in those three files or a lint-policy ruling.

TWO-STEP LANDING CONSTRAINT (new, and it bites from both directions). The old
engine cannot boot the converted tree and the new engine cannot boot the
unconverted one. Build in two passes: (1) reorder + effects files, install;
(2) then the forwarder deletions, install. Measured both ways - the old
binary dies E-UNDEFINED on a deleted forwarder's caller, and the new binary
dies "hb: cannot open src/core/util-effects.f" on master's tree. Any future
attempt to verify a claim against master from this lane needs a
master-compatible bin/hb copied in first.

MODEL GAP, stated rather than weakened. The render-side defer wall gives
TFAM-N-XT a registry-not-loaded default of 0, and I could NOT falsify it by
mutation: deleting the default changes nothing observable, because
TFAM-RESOLVE-XT's own default refuses every family term first, so no T-PARAM
can exist in the check-hook.f -> type-family.f:3220 window and FAM-NAME-REND
is never reached. The default is argued from the call graph, not proven. Two
builds are on the record: with and without it, identical behaviour, and a
planted `: ZZ-FAMPROBE ( extprod<a> -- ) ;` in the window dies at "unknown
type 'extprod' in signature" rather than at the renderer. Whoever re-derives
this either finds the reaching path or deletes the default and lets the hook
fail closed on DEFER-UNSET like its documented siblings.

WHAT IS STILL OWED IF AND WHEN THIS RESUMES: the CODELEN ratchet in
test/gate-size-attribution-test.f and the two child-process counts in
test/internal-word-gate.f (PARITY-DIRECT-N 9->10, PARITY-SUBJECT-N 103->102)
need re-measuring, and test/run.f stays red until they are. They were left
deliberately - finding 1 changes the tree, so measuring them now would be
measuring a tree nobody is going to land. The full test/run.f on the candidate
had 5 red phases and every one is accounted for above; none was unexplained.
The recovery leg was not run for the same reason: bootstrap.sh's manifest is
mirrored and `bash -n` clean, but a recovery build cannot be honestly judged
until the design settles.

FOR THE NEXT LANE, AND THIS IS THE POINT. Every gate this change had to pass
would have passed it. The byte-identical fixpoint passed, maki passed, the
diff lints passed, and the 95-row deletion looked exactly like the win the
epic asked for. What stopped it was a worker reading a failing assertion as
the tree telling it which dot it was reversing, instead of updating the
assertion. That is the fourth time this campaign a lane's stop saved a merge
every gate would have approved. The stop discipline is the campaign's best
instrument - use it.

LESSONS (verbatim, for LESSONS.md at merge):
- **Recording a signature is the same act as publishing an API.**
  `internal-mark.f` welds "checker-known" to "top-level executable", so any
  move that makes a file checked publishes every global it defines. Count the
  globals before scoping such a move.
- **A scout's "costs nothing" about a file is about its CONSUMERS; measure its
  BILL separately.** `layout-valid.f` had zero pre-hook consumers and 29
  pre-hook dependencies.
- **`undefine` retires a dictionary entry, not a recorded row.** A seam
  designed as a `defer` specifically to stay retirable loses that property the
  moment its file is checked.
- **`jj edit <old-commit-id>` after the change was rewritten silently checks
  out the stale snapshot and creates a divergent change.** Two edits were
  reverted under me and only a marker sweep caught it. Use the change id, and
  sweep for your own markers after any `abandon`/`edit` cycle.
- **A gate assertion that fails after your change is often the tree telling you
  which dot you are reversing.** `using-test.f`'s FRESH precondition and
  `internal-word-gate.f`'s CT-LIVE? case both named their own obsolescence
  condition in advance.
