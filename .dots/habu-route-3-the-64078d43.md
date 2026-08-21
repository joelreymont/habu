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

Claim: agent=route3-5 workspace=.jj-ws/habu-trusted (TAKEOVER 2026-08-21 from
route3-4, which stood down at depth with the mechanism landed and step 2's
census measured but its first conversion batch failing for an unknown reason -
see BANKED BY ROUTE3-3 for the first lane's record, MEASURED BY ROUTE3-4 for
the attribution pass, and SOLVED BY ROUTE3-5 at the end of this leaf, which
supersedes §8's reading of the failure AND §7's interpret-route residue.
ROUTE3-5 STOOD DOWN AT DEPTH after banking SCHEMA-REG's unit; §19 is the
successor's first commit and the claim is open for takeover)

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

BANKED BY ROUTE3-3 (2026-08-21). Route 3 is BUILT AND GREEN ON ITS OWN GATES,
under a user ruling that supersedes both shapes the earlier lanes argued. The
chain is three commits in .jj-ws/habu-trusted, NOT pushed, NOT merged:

    3effe189  dots: route 3 resumes past the seals, boot-pin's packaging gets
              its owner
    b9111615  boot-pin: the prefix list gets a package
    ba2900e7  codegen: check the type foundation past the hook

THE HEADLINE, unchanged and re-measured: src/'s TRUSTED: census goes 218 -> 123,
NINETY-FIVE rows deleted, 75 of them answerable only by route 3's recorded rows
(4 more were already answerable through an existing axiom - a seal dividend, not
route 3's). Second: the DERIVE-SET specimen below is a registry mutator DELETED,
not hidden.

STATE: crossing build rc 0; self-check census `0 uncheckable, 0 rejected,
certified = 4728`; fixpoint byte-identical across two generations
(2db1520772ad992f...); both diff lints 0 over master::@; error-code-lint 0;
dot-dep-lint 0; maki rc 0 zero FAIL/RED; bootstrap-mirror-lint 0; `bash -n
tools/bootstrap.sh` clean.

--- 1. THE RULING'S MODEL, AS EXECUTED ---------------------------------------
Private by default; a sibling that needs an internal EXTENDS BY REOPENING; the
package SEALS after its last prefix extender; whatever stays public is genuine
API and is checker-known and callable.

The unverified mechanic - a package'd sibling doing the sequential
close/reopen-theirs/extend/close/resume dance at prefix scale - IS PROVEN. It
works, it certifies, and it costs one definition. Worked example, and the
specimen a successor should copy:

  src/core/type-family.f:478 area  the two raw derive-bit setters
      TFAM-DERIVE-EQ! and TFAM-DERIVE-HASH! were PUBLIC and wrote a family
      record with NO guard of any kind. `: R33-DERIVED ( n -- )
      TFAM:TFAM-DERIVE-EQ! ;` CERTIFIED from ordinary user source - measured.
      They are now DELETED outright, because once the extension existed neither
      had a caller left. TF.DERIVE stays private.
  src/core/sumtype.f:700 area      the extension block, verbatim shape:
      ;package / package TFAM / public / : DERIVE-SET ( n n -- ) {: fam:n
      bit:n :} fam TF-REC@ TF.DERIVE dup @ bit or swap ! ; / ;package /
      package TYPE-DECL / private
      Packages do not nest (habu2.f C-PACKAGE exits $4B) so the blocks are
      sequential; reopening TYPE-DECL resumes the same two wordlists it left.
  consumers  src/core/sumtype.f, structure-decl.f:277-278,
      enum-decl.f:307-308 call `fam DRV-EQ DERIVE-SET`.

THE FIVE CONTAINMENT LEGS, all measured on the built engine:
    TFAM:DERIVE-SET from a checked body ......... certifies, callable
    private bare TFAM-DERIVE-EQ! ............... E-UNDEFINED
    private qualified TFAM:TFAM-DERIVE-EQ! ..... E-UNDEFINED
    private through `using TFAM` ............... E-UNDEFINED
    production ENUM r33col DERIVE eq ... ;ENUM . works

A NAMING NOTE THE NEXT SWEEP WILL HIT: moving a public to private puts its
definition line in the diff, and package-diff-lint then reports
E-REDUNDANT-PACKAGE-PREFIX (`TFAM-DERIVE-HASH!` repeats its owner `TFAM`).
Count callers before renaming - a word that was public solely for one caller
usually has none once that caller is served properly, and deletion beats a
rename.

--- 2. SEAL STATE, AND 113ecd89 --------------------------------------------
`package X private ... ;package` from an ordinary `bin/hb --load` program:

    package      before        after      closed by
    TFAM         rc 84         rc 84      RESTAB (habu2.f KWDATA:RESTAB-BUF)
    LOWER-CERT   rc 84         rc 84      RESTAB
    SCHEMA-REG   rc 0 REOPENED rc 84      this lane's ceremony
    TYPE-DECL    rc 0 REOPENED rc 84      this lane's ceremony

So HALF the registry packages were already closed against reopen for a reason
that has nothing to do with sealing - measure the reopen route per package
before writing a ceremony. The two that needed it now carry src/habu/xref.f's
ceremony (`private get-current prot-wid-add public get-current prot-wid-add`),
placed before the closer of the LAST block that opens the package:
src/core/type-schema.f tail, and src/core/sumtype.f BEFORE its `;package` at
the TDECL-TXN-XT line - NOT at end of file, where the seven global grammar
words live outside the package (that mistake is exit 75 printing the bare token
`private` and nothing else).

The two habu-pkg-reopen-reaches-113ecd89 WITNESS PROGRAMS, both previously
rc 134 SIGSEGVs, are now refused AT THE REOPEN with rc 84, before the poke:
    package TYPE-DECL / private / 0 TDPLAN-P ! / ;package / SUMTYPE ...
    package SCHEMA-REG / private / 0 SCH-RBF-P ! / ;package / SUMTYPE ...
STILL OWED: these two witnesses and the four reopen-after-seal legs exist only
as scratch probes in the lane's scratchpad. They must become REGISTERED cases
(test/internal-word-gate.f is their home) before this lands - a passing probe
is not a scheduled test.

--- 3. RECLASSIFICATION CENSUS (build step 1) -------------------------------
TFAM, 221 publics, measured by consumer:
    59  no consumer outside src/core/type-family.f at all  (the inner-package
        class the seal leaf predicted: TYPE-NAME, TYPE-FIELD, TYPE-FIELD-OWNER,
        CHECKER-DECL-FRAME, PREFIX-BOUND read the registry through the file's
        own `using TFAM`)
    14  consumed ONLY by prefix sibling implementation files
    ---
    73  PRIVATE CANDIDATES of 221
Consumer-count histogram over the rest: 62 publics have exactly one consumer,
32 have two, 14 three, 14 four, and the tail runs to one name with 22. The 14
sibling-only names: DRV-EQ DRV-HASH PF-FAM-LIVE? PF-NAME-REQUIRE
TDECL-FIELD-CLEANUP-XT TDECL-FIELD-RELEASE-XT TF-GRAMMAR-KEYWORD? TF-UPPER-C
TFAM-DERIVE-ANY? TFAM-DERIVE-EQ! TFAM-DERIVE-HASH! TFAM-DERIVED-TAIL?
TFAM-REWIND TFCL-NODE? (the last has TEST consumers - tests are not
implementation, check before privatising it).
ZERO TFAM publics are consumed by enum-decl.f alone, which is why the probe
case had to come from the sibling-only set.
SCHEMA-REG (63 publics) and TYPE-DECL (45 publics) are UNCENSUSED.

--- 4. THE 13 RED ASSERTIONS ------------------------------------------------
test/internal-word-gate.f fails 13 assertions on the candidate: ordinals 36,
52, 389, 393, 425, 426, 427, 429, 430, 431, 443, 467, 507. Two of them (425 and
429, both `expected 70 got 0`) are the class route3-2 documented in its own
internal-word-gate hunks: T-RES and CT-LIVE? gain rows because a CHECKED
consumer needs them, so the marking pass stops hiding the name and guards it on
declared input depth (DNAME-MIN-IN) instead. THE OTHER ELEVEN ARE UNATTRIBUTED.
A successor must attribute each one before touching any of them - updating a
number you have not explained is how this campaign's worst near-miss happened,
and `internal-word-gate.f` has twice now named its own obsolescence condition
in advance. Under the ruling these re-derive to pin the new containment:
privates unreachable by every route INCLUDING reopen-after-seal, publics
callable, protected cells refused.

--- 5. BATTERY STILL OWED ---------------------------------------------------
test/run.f UNSANDBOXED (the orchestrator's sandboxed shell mass-fails
spawn-heavy suites), judge 46, schedule-lint, the snapshot suites, the
bootstrap recovery leg, and test/using-test.f (the banked commit has hunks for
it that this lane deliberately did not apply). NOT an item: the
tools/hb-build-lib.f "working-copy artifact" from the lane's own report was a
truncated `jj st` misread - the file's manifest reorder is legitimate content
of ba2900e7, and the working copy is clean against its commit.

--- 6. LESSONS, verbatim for LESSONS.md -------------------------------------
- **`public`/`private` outside an open package is exit 75 with the bare token
  as the whole message.** A seal ceremony appended to a file's TAIL lands after
  `;package` when the file has a global surface below its package block
  (sumtype.f's seven grammar words). Place a seal before the closer of the LAST
  block that opens the package, not at end-of-file; the giveaway is a boot that
  prints `private` and nothing else.
- **Two of the four registry packages were already closed against reopen, for a
  reason that has nothing to do with sealing.** `tfam` and `lower-cert` are in
  `KWDATA:RESTAB-BUF`, so `package TFAM` from user source is refused before any
  lookup. Measure the reopen route per package before writing a ceremony - half
  of them may already answer, and the ones that do answer with rc 84 and the
  package name as the entire diagnostic.
- **A cross-file package extension is the way to keep a mutator private and
  still reach it.** Closing your own package, reopening the owner, defining a
  narrow entry where its privates are in scope, then resuming, works at
  boot-prefix scale and costs one certified definition. It converts "this
  internal must be public because a sibling calls it" into "this internal stays
  private and the sibling defines the API it actually needs."
- **When making a word private trips `E-REDUNDANT-PACKAGE-PREFIX`, count its
  callers before renaming it.** The prefix is only redundant because the name
  was designed for global reach; a word that was public solely for one caller
  usually has zero callers once that caller is served properly, and deletion
  beats renaming.

Earlier lessons from this lane, already stated above in context: PPRIM: cannot
reach a package private (it interns through SYM-PUBLIC, so a checked consumer
of a pre-hook package's PRIVATE needs a TRUST row in that package's own private
section - lower-cert-base.f's tail carries 15); a `using` line is load-order
sensitive in a way a call site is not (converting every call in render.f to a
defer did not free it from `using TFAM`, which fails the boot outright with
`hb: using: unknown package: TFAM`); and a reorder script that moves list
entries must move their separators too (tools/bootstrap.sh's `printf '\n'`
lines were left behind, gluing five concatenated sources together, and the file
still passed `bash -n`).

MEASURED BY ROUTE3-4 (2026-08-21). The eleven unattributed assertions are
attributed, and attributing them turned up a bigger fact than the assertions
themselves. Every number below came off the two engines side by side: the
candidate in .jj-ws/habu-trusted and master 81d88a3a in the main worktree,
same probe program, same day.

--- 1. THE THIRTEEN ASSERTIONS, EACH TO ITS MECHANISM -----------------------
Ordinals were located by running the gate with a counter print at every phase
boundary, then counting the assertions inside the phase word; every one was
then confirmed by running its child program by hand. The gate file itself was
not edited.

  ord  phase / case                      what actually happens
   36  NEG-BARE, bare T-RES              rc 70, but the message is now
                                         "interpret stack underdepth: T-RES"
   52  NEG-BARE, bare CT-LIVE?           same, for CT-LIVE?
  389  CTLIVE-CASES, CT-LIVE? --load     same
  393  CTLIVE-CASES, CT-LIVE? stdin      same
  443  SEAL-CASES, SCHEMA-REG:SCHEMA-A@  same
  467  TFAM-SEAL-CASES, TFAM:PF-FIND     same
  507  TFAM-SEAL-CASES, TFL-CVAR?        same
  425  QUAL-CASES, using SCHEMA-REG      the child EXITS 0. 426 and 427 are the
  426                + bare REWIND       two message checks failing behind it.
  427
  429  QUAL-CASES, 0 0 SCHEMA-REG:REWIND the child EXITS 0. 430 and 431 are the
  430                on --load           two message checks failing behind it.
  431

TWO MECHANISMS, NOT ONE. The seven "underdepth" rows are the class route3-2
described: the name became checker-known, so src/core/internal-mark.f stopped
setting DNAME-INT on it and poked DNAME-MIN-IN instead. Still refused, still
rc 70, different sentence. T-RES and CT-LIVE? are checker-known because this
change adds a PRIM: row for each in checker.f; SCHEMA-A@, PF-FIND and TFL-CVAR?
are checker-known because their own definitions now record a signature.

THE SIX rc-0 ROWS ARE NOT THAT CLASS, and route3-3's leaf attributes them to it
in error. They are the habu-pkg-publics-escape-41532ee7 defect coming back:
SCHEMA-REG:REWIND takes two inputs, the child program supplies two, so the
depth guard has nothing to refuse and the word RUNS. Witness, on the candidate:

    SCHEMA-REG:SCHEMA-N@ . cr      \ 50
    0 0 SCHEMA-REG:REWIND
    SCHEMA-REG:SCHEMA-N@ . cr      \ 0
    SUMTYPE r34wipe 0 VARIANT r34a n ;VARIANT ;SUMTYPE   \ rc 76, bad schema node

On master the same program answers "hb: internal engine word: SCHEMA-REG:REWIND"
and rc 70. Anybody updating those six numbers without running the program would
have written down that a live registry wipe is expected behaviour.

--- 2. HOW BIG THE HOLE IS, MEASURED NOT ESTIMATED --------------------------
Finding 1 is reduced, not closed. Probe: every public of the moved block (from
tools/public-signatures.f, which reads the real sources and reports each word's
declared inputs) plus every PRIM:/PPRIM: row this change adds to checker.f -
369 names. Each was called from an ordinary `bin/hb --load` program with its
declared inputs supplied as zeros, on the candidate and on master.

  288 names that master REFUSES are REACHABLE on the candidate:
      231 run to completion, rc 0
       28 reach an engine die, rc 76
       25 reach an uncaught throw, rc 67
        4 take the process down with a register dump, rc 134

  by owner:   TFAM 132   checker.f globals 68   SCHEMA-REG 42
              layout-buffer.f globals 26   TYPE-DECL 18   TYPE-NAME 2

The four crashes are the c5be6634 class this gate exists to prevent, and all
four are refused on master:
    0 0 0 REG-GROW1            rc 134   (checker.f)
    0 0 0 REG-PERSIST-BUF      rc 134   (checker.f)
    0 0 0 0 LBUF-VALIDATE      rc 134   (layout-buffer.f)
    0 0 0 0 STORAGE-VALIDATE   rc 134   (layout-buffer.f)
Also newly handed to user source: raw arena base pointers (TFAM:PF-BASE,
SCHEMA-REG:SCH-BASE, SCHEMA-REG:SCH-ROOT-BASE, TFAM:TF-RBF-CUR) and whole-
registry resets (TFAM:TFAM-RESET, SCHEMA-REG:SCHEMA-RESET).

THE OWNING SOURCE STATES THE RULE ITSELF. src/core/internal-mark.f: a record
with no checker-known effect gets DNAME-INT and is refused; one with a
checker-known effect and declared inputs above zero gets DNAME-MIN-IN and is
refused only on a short stack; one with a checker-known effect and no declared
inputs gets nothing at all. Its stated aim - "the executable top-level name
universe equals the checker's" - held while only a curated public API was
checker-known. This change makes 662 implementation definitions checker-known,
so the same rule admits all of them.

--- 3. WHAT THE SPECIFIED SWEEP CAN AND CANNOT REACH ------------------------
Consumer census over the 194 reachable names that belong to a package, counting
references through the tree's own lexer (tools/lint/source-lex.f, so a name in
a comment or a string is not a reference) across all 1519 .f files:

    59  referenced nowhere outside their own file      -> private, or delete
    83  referenced only by prefix siblings in src/core -> private + the
                                                          DERIVE-SET extension
    46  referenced only by TESTS                       -> needs a ruling
     6  referenced outside src/core                    -> genuinely public
   ---
   194

So the sweep reaches 142 cleanly and 46 more depend on whether a test may reach
a registry internal. The remaining 94 are NOT package publics at all - they are
globals in src/core/checker.f (68) and src/core/layout-buffer.f (26), both of
which docs/forth.md admits by exact path as global surfaces. Making a word
private is not available to either file, and PPRIM: cannot reach a package
private in any case. Those 94 include all four crashes.

--- 4. THE TREE DOES HAVE A CEREMONY THAT WOULD WORK ------------------------
Falsified, not assumed. src/core/util.f REG-PROTECT records the just-defined
record's dictionary index, and internal-mark.f IMK-SEAL-REGISTRY int-marks every
index in that list after the classifying walk. Its comment scopes it to data
cells, but the mechanism reads a dictionary index and nothing else. Adding one
REG-PROTECT after `: PF-BASE ( -- ptr a ) PF-A-P @ ;` in type-family.f and
rebooting turns the candidate's rc-0 `TFAM:PF-BASE . cr` into
"hb: internal engine word: TFAM:PF-BASE" rc 70, while the engine still boots
and a real SUMTYPE declaration still registers (family count 120, rc 0) - the
compiled callers and the checker never consult that flag. The probe was reverted;
the working copy is clean.

So the residue is answerable with a mechanism the tree already ships. What it is
NOT is a decision this lane may take: 94 REG-PROTECT rows is a hand-kept list
where the ruling's own words ("private by default; whatever stays public is
genuine API") point at a rule instead. The three shapes worth weighing are one
REG-PROTECT row per residual name; packaging layout-buffer.f and checker.f,
which is a docs/forth.md exact-path policy change plus the checker-sealing dot
habu-seal-the-checker-5314c0ab; or giving internal-mark.f a second input so a
file can declare its definitions to be implementation rather than top-level API.

--- 5. WHY THIS LANE STOPPED HERE -------------------------------------------
The reclassification sweep is worth doing and is tractable - 142 names with no
argument to have about them. It is not sufficient. Running it first would leave
94 reachable globals and four process crashes behind, pass every gate the
banked chain already passes, and look finished. That is the shape this campaign
has been caught by four times, and the leaf above says so in its own words.
Open questions for the orchestrator, in the order they block work:
  1. the 94 globals - which of the three shapes above;
  2. the 46 test-only consumers - may a test reach a registry internal, or does
     the test move to the public entry;
  3. the six genuine external consumers (TFAM:SUMV-CTOR-SYM@,
     TFAM:TFAM-SIG-RESOLVE, TFAM:TFAM-ACTIVE-PKG$, TYPE-DECL:TDPLAN-BEGIN,
     TYPE-DECL:TDPLAN-DEF$, TYPE-DECL:TDECL-CTOR-WORD) - API, or consumers to
     convert.
Nothing in the banked chain was changed by this lane. The gate assertions were
NOT updated: five of the thirteen describe behaviour that is currently wrong,
and the number to write down depends on answer 1.

--- 7. THE RULINGS EXECUTED, AND THE SECOND HOLE (route3-4, 2026-08-21) ------
RULING 1 IS BUILT AND IT WORKS. src/core/util.f gains three words next to
REG-PROTECT - IMPLEMENTATION, ;IMPLEMENTATION and API - recording a [from, to)
dictionary span per declared file plus the records in it that are interface;
src/core/internal-mark.f gains IMK-SEAL-IMPLEMENTATION, which sets DNAME-INT on
every other COLON record in each span whatever the checker knows of it. Four
files declare themselves: checker.f (138 API), type-family.f (51),
sumtype.f (7), type-schema.f (2), layout-buffer.f (6), render.f (3), util.f (5).

  Top-level name universe, candidate against master 81d88a3a, whole dictionary
  including package publics, read out of the engine's own record array:
      residue ............... 0   (was 288)
      API regressions ....... 0
  test/internal-word-gate.f: ALL THIRTEEN ASSERTIONS GREEN, no test edited.
  The four register-dump programs, both registry wipes and the arena-base read
  all answer `hb: internal engine word`, rc 70.

THE FAIL-CLOSED PROOF THE RULING ASKED FOR, measured: a definition added inside
a declared file with NO other edit is refused -
`: R34-NEW-DEFINITION ( n -- n ) 1 + ;` in layout-buffer.f answers
`hb: internal engine word: R34-NEW-DEFINITION` rc 70; the same line followed by
API prints 4 and exits 0. A list of protected names cannot do that, which is why
the 94 REG-PROTECT rows were the wrong shape.

RULING 2 COST NOTHING. No suite needed a TRUSTED: shim: DNAME-INT is read by
interpret dispatch and interpret tick only, so a test that calls a registry word
from inside a definition is untouched, and none called one at top level.

RULING 3, the six recorded as the moved block's intentional API surface:
TFAM:SUMV-CTOR-SYM@, TFAM:TFAM-SIG-RESOLVE, TFAM:TFAM-ACTIVE-PKG$,
TYPE-DECL:TDPLAN-BEGIN, TYPE-DECL:TDPLAN-DEF$, TYPE-DECL:TDECL-CTOR-WORD. Each
is decl-machinery its consumers legitimately name.

THE SECOND HOLE, AND IT IS THE WORSE ONE. DNAME-INT guards the interpret route
by design and says so; it does not guard CHECKED code, and the checker never
consults it. Being post-hook publishes every package PUBLIC of the three
registry files to checked user source under its qualified name. Measured the
same way, 351 package publics probed as `: X ( -- ) PKG:NAME drop drop ;` on
both engines:

    291 package publics reachable from CHECKED code on the candidate and
        E-UNDEFINED on master   (TFAM 194, SCHEMA-REG 61, TYPE-DECL 34,
        TYPE-NAME 2)

and that includes the raw registry cells. util.f's own comment names the exploit
it was written to stop; here it is, through the checked path, on the candidate:

    : R34W ( -- ) $63 TFAM:PF-COMMIT-N ! ;
    TYPE-FIELD:COUNT . cr     \ 48
    R34W
    TYPE-FIELD:COUNT . cr     \ 99      exit 0

Master answers `E-UNDEFINED habu: in r34w: undefined word 'TFAM:PF-COMMIT-N'`
and rc 70. The ONLY closure for this route is package `private` - which is the
`public -> private` half of the ordered sweep, and it is therefore not
optional and not deferrable. Consumer census for those names, counted through
the tree's own lexer over all 1519 .f files:
     65  reachable only from files that already REOPEN the package - private
         costs those consumers nothing
    129  reached through `using PKG` or a qualified call and need the
         DERIVE-SET conversion (58 of them from tests only)
The 42 SCHEMA-REG names in that second group are reached by six prefix siblings
through `using SCHEMA-REG`; converting them means those siblings reopen the
package, which is expressible but is a restructuring of the boot prefix rather
than an edit.

ONE RED IS LEFT AND IT IS THIS HOLE'S SYMPTOM, not a separate defect.
test/type-field-owner-suite.f assert 188 is
`s" TFO-N3 ( -- n ) PF-COMMIT-N" NOT-RETIRED`: the fixture was written when
PF-COMMIT-N was unmodelled, so the checker answered 1 (uncheckable) and the
assertion passed. The cell is checker-known now with effect `-- ptr n`, so the
fixture's declared `( -- n )` is a type error and the verdict is 0. Correcting
the fixture's effect would make it green while writing the defect down as
expected, so it is left red and named here. It goes away when PF-COMMIT-N
becomes private, and its REGISTRY-CASES sibling then moves from TF-CELL-PUB to
TF-CELL-PRIV - a strengthening the model asks for.

STATE OF THIS CHAIN: crossing fixpoint build + install rc 0 (roundtrip ok, two
processes wrote the same artifact); internal-word-gate green; maki rc 0 zero
FAIL/RED; both diff lints 0; error-code-lint 0; dot-dep-lint 0; type-decl,
type-family, type-family-rollback, type-export, enum-decl, structure-decl,
boot-pin and using suites all rc 0. NOT LANDABLE while assert 188 is red.

LESSONS (verbatim, for LESSONS.md at merge):
- **A gate ordinal is not a mechanism.** Two of thirteen failures carried the
  same message and came from opposite causes - one a diagnostic reworded, one a
  registry mutator running to exit 0. Print the assertion counter at each phase
  boundary to locate the ordinal, then run its child program by hand; a
  predecessor's attribution is a claim, not a measurement.
- **"The checker can type it" and "a user may type it" are two facts, and
  internal-mark.f read one off the other.** That was sound only while the
  checker knew a curated surface. Any change that makes an implementation file
  checked publishes it, so the file has to say which of its definitions are
  interface - default closed, published by an act at the definition site.
- **DNAME-INT guards the interpret route only, and that is written in the file.**
  Closing every top-level spelling still leaves checked code reaching a package
  public. Only `private` closes both routes; measure the checked route
  separately or you will believe a hole is shut when half of it is open.

--- 8. STEP 2 STARTED, AND WHERE IT STOPPED (route3-4, at depth) -------------
CENSUS OF THE CHECKED ROUTE, which is what step 2 has to close. Of the 291
package publics reachable from checked user code on the candidate and
E-UNDEFINED on master:
     71  reachable only from files that already reopen the package
         (TFAM 64, TYPE-DECL 7) - the set that should cost nothing
    220  need a conversion (SCHEMA-REG 61, TFAM 130, TYPE-DECL 27, TYPE-NAME 2);
         126 of those have test-only consumers, 94 production ones
Production consumer files, by how many names each reaches: type-family-suite 51,
type-family.f 38, enum-decl.f 30, structure-decl.f 27, sumtype.f 26,
type-decl-suite 26, type-ctor-suite 17, type-family-rollback-suite 16,
layout-valid.f 14, structure-make-suite 12, enum-decl-suite 11,
structure-decl-suite 11, structure-make.f 11, then a tail of 30 files at 10 or
fewer. The full per-name table is the lane's sweep291.json; regenerate it with
tools/lint/source-lex.f rather than trusting this summary.

THE 71 "FREE" CONVERSIONS ARE NOT FREE, AND THIS LANE DID NOT KNOW WHY.
Applied mechanically the prefix stops loading with `E-UNDEFINED: TF.NAME-OFF`.
Attributed in §9-§11 below: the census counted files, not scopes, and the
failure is at type-family.f:448, not at the layout self-check. The lead this
section left - instrument the checker's package mirror - is a DEAD END; do not
spend the hour. The sweep edit was REVERTED; nothing of it is in the banked
commit.

WHAT THE SUCCESSOR INHERITS, all in this workspace: the mechanism (landed and
green), the two residue measurements and the tooling that produces them -
a dictionary dumper that reads the engine's own record array for both routes,
a checked-body prober over every package public, and a structural token dumper
built on tools/lint/source-lex.f. Reproduce the landing's numbers before
touching anything: interpret-route residue 0, API regressions 0, and the
checked-route figure 291, which is step 2's whole job.

SOLVED BY ROUTE3-5 (2026-08-21). The mystery is attributed, §8's lead was a
dead end, §7's interpret-route residue was wrong by ten names and is now really
zero, and the sweep §8 specifies does not exist in the shape it specifies. Every
number below came off the two engines side by side, candidate in
.jj-ws/habu-trusted and master 81d88a3a in the main worktree.
--- 9. THE MYSTERY, ATTRIBUTED ----------------------------------------------
Not the checker's mirror, and not the top-level self-check. Markers at every
top-level statement show `0 TF.NAME-OFF TF.NAME-OFF-AT TF-LAYOUT=` PASSING with
the name private; the load dies eleven lines later at type-family.f:448,
`: TFAM-NAME$`, which sits between the `;package` on 443 and the `package TFAM`
on 451 - one of the SIX engine-required GLOBALS docs/forth.md admits by exact
path - and reaches the registry through the file's own `using TFAM` on 445.
`using` imports a package's PUBLIC wordlist only, so a private TF.NAME-OFF is
correctly invisible there while the three identical lines above it read names
from a scope where TFAM is OPEN. NO CHECKER MISS: the bad program fails loudly
on its load path (rc 70) and test/using-test.f:84 already pins the shape.
THE INSTRUMENT WAS THE DEFECT: privatisability is a property of every REFERENCE
SITE's open package scope, not of the FILE the reference sits in.

--- 10. THE MUTATION TABLE --------------------------------------------------
Synthetic, one variable at a time: a private referenced in the SAME open block or
in a REOPENED block of its owner is rc 0; the same private referenced from a
SIBLING package under `using`, or at GLOBAL top level under `using`, is rc 70
E-UNDEFINED, and the identical case with the name PUBLIC is rc 0. Real prefix
names privatised alone and booted (bin/hb re-reads its core prefix, so no build
is needed): TF.NAME-OFF, PF-FIND, PF-ALIGN@, PF-COMMIT-N, TF-REC@ and
type-schema.f SCH-BASE all rc 70, each on ITS OWN name at the predicted site.
sumtype.f TDPLAN-BEGIN rc 0 - its only closed-scope sites are in
test/type-ctor-suite.f and tools/decl-gen-probe.f, which boot never loads, and
running that suite under the same mutation gives rc 70 against a baseline rc 0.
The scope-aware census predicts the engine on both routes; the file-granular one
does not.

--- 11. THE CENSUS, REDONE ON SCOPE -----------------------------------------
Same structural token stream (r34-tok.f -> tools/lint/source-lex.f, all 1519 .f
files, 1,255,527 word tokens), each reference site classified by the package OPEN
at that token. Re-running §8's FILE-granular rule over the same data reproduces
its number exactly - 71 free, TFAM 64, TYPE-DECL 7 - so the pipelines agree and
only granularity differs. On scope: of the 291 checked-route names 0 are free, of
§8's 71 "free" names 0 are free, and of all 351 package publics 0 have no
closed-scope reference. By where the closed-scope references live: TFAM 131
prefix / 63 test-tool, SCHEMA-REG 45/16, TYPE-DECL 17/17, TYPE-NAME 2/0 -
195 prefix and 96 test-tool of 291. The prefix side is thirteen packages across
nine files sharing one registry, and prefix-scope consumers cost nothing under
retirement because they compile first.

--- 12-13. WHY THE SWEEP COULD NOT EXPRESS THESE, AND WHAT CLOSES IT --------
The DERIVE-SET shape serves a sibling needing one composite OPERATION. It cannot
serve a sibling that is the registry's own PUBLIC VIEW: `package TYPE-FIELD`
(type-family.f:2270-2308) is fifteen one-line forwarders, `: ALIGN@ ( n -- n )
PF-ALIGN@ ;` and fourteen like it, each already marked API. TYPE-FIELD:ALIGN@ IS
the API and TFAM:PF-ALIGN@ IS the implementation, and nothing moves the forwarder
into TFAM's private scope: reopening TFAM renames the public and cascades its
callers, EXPORT republishes under the same tail so it cannot rename, packages do
not nest. TYPE-FIELD-OWNER, CHECKER-DECL-FRAME, PREFIX-BOUND and TYPE-NAME are
the same shape. Capability dotted and closed at habu-a-pkg-needs-92764584.
WHAT CLOSES IT INSTEAD: the checked route has exactly two entries and both read
the package PUBLIC wordlist - qualified `TFAM:X`, and bare `X` under `using TFAM`,
which ordinary user source may open (measured: `using TFAM : W ( -- )
$63 PF-COMMIT-N ! ;` runs, TYPE-FIELD:COUNT goes 48 -> 99, exit 0). So §7's "only
private closes both routes" survives re-derivation, but removal need not be a
SOURCE edit. §16 builds the pass that does it.

--- 14. §7's INTERPRET RESIDUE WAS 10, NOT 0; IT IS 0 NOW -------------------
Measured differentially over the whole record array: ten names open on the
candidate and ABSENT from master, all data cells of the sealing mechanism itself
- IMPL-SPAN-CAP/FROM/TO/N, IMPL-OPEN, IMPL-API-CAP/IDX/N (util.f) and
IMK-S/IMK-J (internal-mark.f). Not a claim: `IMPL-SPAN-N @ . cr` printed 7 and
`$63 IMPL-SPAN-N !` then printed 99, from ordinary user source. util.f's own
header says what closes it, so each cell now carries that row. After the fix,
`hb: internal engine word: IMPL-SPAN-N` rc 70; §7's fail-closed proof still
answers. REG-PROT-N is 71 of REG-PROT-CAP 192.
PRE-EXISTING AND NOT ROUTE 3's: REG-PROT-N, REG-PROT-IDX, REG-PROT-CAP and IMK-I
are open and writable on MASTER too - REG-PROTECT tags the just-defined record
and structurally cannot reach records defined before itself. Dotted at
habu-reg-protect-cannot-587ee552; not touched here.
SCHEDULED, not merely passing: test/internal-word-gate.f IMPL-CELL-CASES is one
case per cell plus the measured write and an `' IMPL-API-N` tick, and
PARITY-SUBJECT-N moved 197 -> 209 -> 210. The gate is registered at
test/gate-stdlib-cases.f:1202. Falsified by mutation: deleting IMPL-SPAN-N's row
turns the gate red with six TFAILs, deleting IMK-J's with three, so every row is
pinned by its own case.
The scope-aware census is a lane instrument; the durable form is dotted at
habu-a-pkg-public-99a005d8 and needs a named first consumer before minting.

--- 16. THE RETIREMENT PASS: BUILT AND PROBED -------------------------------
Ruled 2026-08-21: the retirement pass, not the sweep and not a new visibility
capability.

PLACEMENT IS LOAD-ORDER-DERIVED, NOT A CHOSEN ROW. habu2.f PFX-LOAD-INTMARK says
in its own comment that internal-mark.f is the LAST cold-prefix source because
its pass must see every prefix definition. Retirement needs the identical fact
from the other side - every prefix consumer must already have COMPILED, because
`undefine` retires a dictionary entry and not a compiled reference. Same row,
same argument. Verified: src/core/top-row.f loads after it and names none of
these words.

THE PASS NEEDS NO NEW ENGINE WORD. src/habu/xref.f publishes global
UNDEFINE-FOUND ( ptr u8 n n -- ) - name plus dictionary index, no lookup, no
package reopen - at prefix row 907 against internal-mark's 972, and
internal-mark.f already owns IMK-QUAL and IMK-API?. Three words: IMK-IN-SPAN?, a
linear test against IMPL-SPAN-FROM/TO; IMK-RETIRE-PUB, which exits on a record
outside every span or carrying an API mark and otherwise calls
`p i IMK-QUAL i UNDEFINE-FOUND`; and IMK-RETIRE over package rows. IMK-PASS gains
IMK-RETIRE last, after IMK-SEAL-PRIM. Source at the lane scratch as
r35-retire-pass.snippet.

THE PROBE, SCHEMA-REG end to end, on the real engine: 63 publics, 2 API-marked;
61 closed on the CHECKED route and the 2 survivors are exactly the marked pair
(SCHEMA-N@, SCHEMA-ROOT-N@) - the declared surface and nothing else; checked
residue 291 -> 230, the drop exactly SCHEMA-REG; `SUMTYPE r35pk 0 VARIANT r35a n
;VARIANT ;SUMTYPE` registers rc 0 with SCHEMA-N@ reading 51, so the registry is
fully live; the prefix boots and every compiled prefix consumer still runs.
HAZARD SWEEP FIRST, because a code-token census cannot see a name resolved from a
string at runtime and that is exactly what retirement breaks (LESSONS.md, the
type-family-sha incident). Over-approximating scan of every quoted region in all
.f/.fs files for the 322 retire targets: 77 tails hit, and every hit in src/ or
bootstrap/ is a diagnostic message or prose. ZERO name-resolution sites in the
prefix or the recovery emitter. Every genuine hit is a test child program or a
lint fixture - internal-word-gate, type-field-owner-suite, prop-test-core
(TFAM:TFAM-DECL-PARAM-COUNT, the one LESSONS predicted), package-diff-lint-test,
type-family-rollback-suite.
THE DECISION SURFACE IS 130 NAMES, NOT 322: prefix-scope consumers compile before
the pass and cost nothing. Per package, publics / API today / reached by a
non-prefix consumer: TFAM 218/23/63, SCHEMA-REG 63/2/48 (now 37 marked),
TYPE-DECL 45/0/17, TYPE-NAME 2/0/0, CHECKER-TAPE 15/5, PRIM-LINK 4/0/1,
CHECKER-BOUND 3/0/1, CHECKER-PREFLIGHT 3/1/0. TYPE-FIELD 15/15 and
TYPE-FIELD-OWNER 8/8 are already fully declared - the specimen, in the tree today.

--- 17. RULING 2 IS NOT AVAILABLE, AND SCHEMA-REG IS MARKED ----------------
THE SHIM ESCAPE HATCH DOES NOT EXIST. Retirement removes the dictionary entry,
so nothing can name the word afterwards - a `TRUSTED:` shim least of all, since
its body must resolve the name like any other body. Measured against a retired
SCHEMA-REG:SCHEMA-A@, six routes and every one refused: a checked qualified
reference, a TRUSTED: shim on the qualified name, a TRUSTED: shim on the bare
tail under `using`, a `0 set-check` body and `EXPORT` all answer E-UNDEFINED
(EXPORT rc 70), and `package SCHEMA-REG` is rc 84. That is retirement being
stronger than hiding, which is why the ruling chose it - but it means the suites'
EXISTING shims are exactly what breaks: test/type-family-suite.f:105 and
test/structure-make-suite.f:55 already read
`TRUSTED: TWX-SCHEMA-RESET ( -- ) SCHEMA-RESET ;`. That convention answers the
DNAME-INT route, where a shim lets a body call an int-marked word. It has no
answer here. Front door or API mark, and nothing else.

SCHEMA-REG IS MARKED AND LANDED: 37 API of 63, the criterion written as six
lines at the top of the file's block so the next person marks by rule and not by
matching precedent. API is the term vocabulary (SCH-PARAM/CON/APP/QUOT/PTR/ROW,
SCH-QUOT-ROWS), the constructors (SCHEMA-PARAM/CON/APP/ROW/QUOT/PTR, ROOT+), the
readers and predicates (TAG@ A@ B@ C@ N@ ROOT-N@ ROOT@, the six SCHEMA-*?, the
four SCHEMA-ROW-*, the five SCHEMA-QUOT-*@) and the error code E-SCHEMA-BAD.
Implementation and unmarked: the bases, capacity and cursor cells, both layout
triples, SCHEMA-RESET REWIND COUNTS SCHEMA-NEW SCHEMA-NODE-OK?, the rollback
words and SCHEMA-SNAPSHOT-PERSIST.

--- 18. THE THREE ANSWERS, BUILT --------------------------------------------
1. THE LIFECYCLE CONTRACT NEEDED NO DESIGN CHANGE - no stop. The registry
already carried the mark-and-restore pair for its own use: `COUNTS ( -- n n )`
reads the node/root high-water and `REWIND ( n n -- )` puts it back behind a
bounds die, and src/core/type-family.f PREFIX-BOUND:PFX-MARK/PFX-REWIND is the
existing caller. Both stay implementation; the front door is two thin publics
over them, `SCHEMA-MARK ( -- n n )` and `SCHEMA-RESTORE ( n n -- )`, API-marked,
inheriting REWIND's guard so a pair this registry never issued fails closed and a
mark can only be restored downward. The contract's own justification is
measured: without it a suite reaches for SCHEMA-RESET, which returns the registry
to its BOOT base and discards every schema the prefix registered.
2. THE NEGATIVE GUARD PROBE IS NOW A SELF-CHECK in type-schema.f, in the shape
the file already uses for SCH-LAYOUT= - a checked word plus a top-level row,
because control words are compile-only at top level. `SCH-BAD-TAG-REFUSED?`
catches `SCH-KIND-MAX 1 + ... SCHEMA-NEW` and `SCH-GUARD-HELD` dies if the throw
did not come. Falsified BOTH ways: deleting SCHEMA-NEW's tag guard fails the boot
rc 76 with the named message, and weakening the probe to a VALID tag fails it
too, so the row cannot pass by accident.
3. THE INSTRUMENT belongs to TYPE-DECL's landing: decl-gen-probe reaches
TYPE-DECL names, not SCHEMA-REG's, so its extension block lands with that
package.

SEQUENCING FINDING, MEASURED, AND IT CHANGES THE UNIT. SCHEMA-REG cannot be a
standalone green commit for its six red suites, and the reason is in their
fixtures rather than in the marks. test/enum-decl-suite.f:87-95 and
test/structure-decl-suite.f:70-78 hold ONE word, `REG-MARK`/`REG-RESTORE`, that
snapshots NINE raw counters across three registries - TFAM-N TF-STR-U TF-PK-N
SUMV-N LAY-N PF-N PF-COMMIT-N alongside SCH-N SCH-ROOT-N. The four SCHEMA-RESET
callers are the same story: every one is `TWX-TFAM-RESET TWX-SCHEMA-RESET` on
adjacent lines. Converting the SCHEMA half alone leaves a half-converted pair,
which is worse than either end state. So the registry's mark/restore contract has
to land for TFAM and SCHEMA-REG TOGETHER, and the natural unit is the registry,
not the package. TFAM needs the same two words over its own counters.

WHAT THIS COMMIT CARRIES: the contract, the self-check, the 37 marks, and one
gate assertion the declaration obsoletes - SEAL-CASES asserted
`SCHEMA-REG:SCHEMA-A@` answers `internal engine word`, and a declared API name
answers `interpret stack underdepth` instead and runs with its declared input.
Both legs are asserted now, and PARITY-SUBJECT-N goes 209 -> 210 for the second
child. NOT carried: the pass, which retires the unmarked 26 and needs the TFAM
half of the contract first.
INTERPRET-ROUTE CENSUS ON THE INSTALLED ENGINE: 29 names newly reachable against
master 81d88a3a, all 29 DECLARED API; UNDECLARED RESIDUE 0; API regressions 0.

--- 19. TFAM'S HALF: MEASURED, AND ITS ONE OPEN DECISION --------------------
TFAM already carries the machinery, but NOT in SCHEMA-REG's shape, and the
difference is a design choice a successor must make before writing the contract.
  src/core/type-family.f:2546 `TFAM-REWIND ( n n n n n -- )` is the guarded
     restore over five counters - TFAM-N TF-STR-U TF-PK-N SUMV-N LAY-N - and it
     does real work first: TFX-RETIRE unchains the family rows and SVX-TRUNCATE
     drops the constructor heads before the ids go out of range.
  src/core/type-family.f:2745 PREFIX-BOUND:PFX-MARK/PFX-REWIND is the live
     caller. It is a STATEFUL pair over module variables (BTFAM BSTRU BPK BSUMV
     BLAY BPF BPFC BSCH BSCHR), not a value-returning mark, and it carries TWO
     more counters past TFAM-REWIND's five: PF-N and PF-COMMIT-N, restored with
     `BPF @ PF-N @ PF-SCRUB` BETWEEN them because the field rows need scrubbing.
So TFAM's mark is SEVEN counters plus an ordering constraint, against
SCHEMA-REG's two. THE OPEN DECISION: `TFAM-MARK ( -- n n n n n n n )` puts seven
cells on a caller's stack, which docs/forth.md's own factoring rules argue
against; the alternatives are a save/restore FRAME STACK in the owner (the shape
SCHEMA-ROLLBACK-SAVE/RESTORE already uses, and PFX-MARK's own shape), or a
one-cell opaque mark handle. The frame stack is the tree's precedent and needs no
new type; the handle is the shape CLAUDE.md records as unresolved for KV. This is
a real interface decision, not mechanical work, and it is where this lane stopped
rather than start it at depth.
WHAT A SUCCESSOR DOES FIRST: pick the shape, publish the pair over TFAM-REWIND +
the PF pair with PF-SCRUB inside, API-mark it with the criterion header, convert
test/enum-decl-suite.f:87-95 and test/structure-decl-suite.f:70-78 REG-MARK/
REG-RESTORE and the four TWX-TFAM-RESET/TWX-SCHEMA-RESET pairs WHOLE, then TFAM's
marks, then the pass over both packages with PF-COMMIT-N retiring and
test/type-field-owner-suite.f assert 188 strengthening in the same commit.

--- 15. THE THREE RULINGS, AND WHERE EACH ONE STANDS ------------------------
  1. RULED: the retirement pass, not the sweep, not a new visibility capability.
     BUILT AND PROBED (§16). Remaining: the API declaration for 130 names and
     the suite adaptation behind it.
  2. RULED: tests do not reach retired internals, adapting through the shim
     convention or the front door. THE SHIM HALF IS NOT AVAILABLE - §17 measures
     six routes to a retired name and every one is refused, TRUSTED: included,
     because retirement removes the entry rather than flagging it. Front door or
     API mark, and nothing else. SCHEMA-REG is marked (37 of 63) and four suites
     go green on the marks alone; three criterion-(c) names with no front door
     block six more, listed in §17 for ruling.
     tools/decl-gen-probe.f: a maintainer instrument, not a test - it drives the
     declaration generator through TYPE-DECL:TDPLAN-BEGIN and eight siblings.
     With no shim route it has no boundary to carry, so its eight probes are
     stop items unless those names are API or the instrument moves inside the
     owning package. LISTED FOR RULING.
  3. The held red clears inside the retirement landing exactly as ruled -
     PF-COMMIT-N carries no API mark, so the pass retires it, assert 188's
     fixture strengthens to the closed answer and its REGISTRY-CASES sibling
     moves TF-CELL-PUB -> TF-CELL-PRIV. It could NOT have cleared under the
     privatisation sweep: PF-COMMIT-N has 14 closed-scope references inside
     `package TYPE-FIELD-OWNER` and `package CHECKER-DECL-FRAME`. The ruling
     picked the option that reaches it.

STATE OF THIS CHAIN: crossing build + `install --force` rc 0, roundtrip ok, two
processes wrote the same artifact; interpret residue 0, API regressions 0;
checked residue 291 (unchanged - step 2 not done); internal-word-gate rc 0 all
thirteen green plus the new phase; using, type-family, type-decl and boot-pin
suites rc 0; maki rc 0 with zero FAIL/RED; both diff lints rc 0;
error-code-lint 0 finding(s); dot-dep-lint 0 finding(s); schedule-lint 0
finding(s); type-field-owner-suite still red on assert 188 only.
NOT RUN, and deliberately: test/run.f unsandboxed, judge 46, the snapshot suites
and the bootstrap recovery leg. The closing battery measures a tree that is
ready to finalize, and this chain is not - ruling 1 changes the tree, so those
numbers would be numbers for a tree nobody is going to land. The same reason §5
gave, and it still holds.

LESSONS (verbatim, for LESSONS.md at merge):
- **A file is not a package scope, and a census that counts files will lie about
  privatisability.** `src/core/type-family.f` reopens `package TFAM` fourteen
  times and still carries five sibling packages and 21 global-span references
  that reach TFAM only through `using`. Classify every reference SITE by the
  package open at that token; the file-granular answer and the scope-aware answer
  differed on all 71 names it mattered for.
- **`using PKG` is a consumer import, so a bare name under it is a PUBLIC
  reference.** The six engine-required globals in type-family.f sit outside the
  package on purpose and reach the registry that way, which is why privatising a
  name they use breaks the boot at the definition, not at the top-level statement
  three lines above it. Print a marker at each top-level statement before
  believing a reported line number.
- **A predecessor's line number is a claim.** §8 named the top-level layout
  self-check; markers showed that line passing and the failure eleven lines
  later, in a definition. The wrong site sent the lead to the checker's package
  mirror, which has nothing to do with it.
- **The mechanism's own state cells are part of its residue.** IMPLEMENTATION /
  API closed 288 names and left ten of its own control cells open and writable
  from user source. Measure the differential over the WHOLE record array,
  including names that exist only on the candidate, or the new machinery hides in
  the gap between the two dictionaries.
- **A dot leaf is capped at 1024 lines and the diagnostic does not say so.**
  `tools/lint/text.f` SMAX is `$400`; past it the dot lint dies rc 1 printing
  `lint: split result overflow` with no file name. This leaf is at 996. Trim
  superseded sections before adding one, and re-run the dot lint after every
  leaf edit, not only after `dot add`.
- **`undefine` retires a public without breaking compiled callers, and that makes
  a late demotion pass possible.** `s" PKG:TAIL" UNDEFINE-NAME` from
  `src/core/internal-mark.f` leaves the prefix's own compiled consumers running
  and a real declaration registering, while both `PKG:TAIL` and `using PKG` +
  bare answer E-UNDEFINED. Before scoping a rename-and-restructure campaign, ask
  whether the surface can simply be closed after everyone who needed it has
  already compiled.
