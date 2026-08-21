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

Claim: agent=route3-3 workspace=.jj-ws/habu-trusted (STOOD DOWN 2026-08-21 at
context depth, with a BANKED three-commit chain - see BANKED BY ROUTE3-3 below
for the successor pointer and everything that exists nowhere else)

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
