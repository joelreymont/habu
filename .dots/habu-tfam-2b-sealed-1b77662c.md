---
title: "TFAM 2b: sealed system packages + friend latch + provenance"
status: open
priority: 2
issue-type: task
created-at: "2026-07-03T23:36:48.915549+02:00"
blocks:
  - habu-route-3-the-64078d43
---

PLAN.md item 2 (sealing half). Sealed TFAM/TYPE/MATCH packages; boot-latch friend capability set during canonical tools/srclist.f engine load, sealed before any user source (user --source-list never friend). Wordlist-layer guards for the full mutator census (set-current/search-wl/tick/execute/postpone/compile,/XREF*/CHECKER-*/raw stores/atomics/here-allot/cp@/immediate/undefine paths); pointer-provenance rejection for syscall/FFI writers vs protected regions; case-insensitive; native+habu1+Gforth mirrors. Fixtures per item 2 acceptance. Gate 17b. Depends: TFAM 2a.

REOPENED 2026-08-04 (dot-purge): this dot carried `status: active` with no live owner - no `agent=`/workspace claim, or a claim explicitly released. An active dot with no owner is invisible to `dot ready` and holds its id hostage, so the status is now `open` and the dot is free to claim. The TFAM 2b umbrella was dissolved, not delivered (commit 150be3a2f archived TFAM 2b-iii); a claimant must re-derive its leaves before dispatch.

RE-DERIVATION MANDATE (2026-08-20, from lane route3-2's measured stop; ruled by
the orchestrator). This dot now BLOCKS habu-route-3-the-64078d43, and the
blocking reason is a number rather than a worry.

WHY THIS IS THE PREREQUISITE. Route 3 moves the type foundation past the
checker hook so its 662 definitions are derived and checked. It was built,
gated and stopped, because in habu recording a signature IS publishing an API:
src/core/internal-mark.f welds "checker-known" to "top-level executable" by
design ("the executable top-level name universe equals the checker's"). So
checking the registry publishes its GLOBAL definitions as top-level-executable,
checked-callable user API. Counted on the candidate tree, per file:

    src/core/type-schema.f      60 global,   2 packaged
    src/core/type-family.f     359 global,  59 packaged
    src/core/sumtype.f         202 global,   0 packaged
    src/core/layout-buffer.f    32 global,   0 packaged
    src/core/layout-valid.f      0 global,  46 packaged
    ------------------------------------------------
    653 GLOBAL definitions become user API if route 3 lands unsealed.

Scope this dot from that table. layout-valid.f is the target state: it is
already fully packaged, and it contributed ZERO to the exposure. Every global
in the other four rows is a row of work for this dot, and the 59 already-
packaged definitions in type-family.f (TYPE-NAME, TYPE-FIELD-OWNER,
TYPE-FIELD) are the shape to extend rather than invent.

THE ACCEPTANCE IS AN EXISTING GATE, NOT A NEW ONE. test/internal-word-gate.f
already encodes the boundary, and it is what caught the exposure: OPENER-CASES
asserts `: IWG-PF-RAW ( n n ptr u8 n -- n bool ) PF-FIND ;` is rejected at
'PF-FIND' - "raw implementation names are not checked/public", the packaged
TYPE-FIELD: reflection surface being the public one (dot
habu-protect-type-field-04d91409). The acceptance for THIS dot is: with the
registries sealed, that assertion and REGISTRY-CASES / SIBLING-CASES stay
green while the foundation is CHECKED. Concretely, this user program must
still be refused after both dots land, and it is the one to keep as the
regression:

    TFAM-N@ . cr
    TFAM-RESET          \ wipes the family registry from user source
    TFAM-N@ . cr
    : LEAK-PF ( n n ptr u8 n -- n bool ) PF-FIND ;

On master it is refused (the registry words are checker-invisible and
DNAME-INT). On route 3's unsealed candidate it runs to exit 0 and prints
119 / 0. On the sealed-then-moved tree it must be refused again, and this time
because the names are PACKAGE-SCOPED rather than because they are unrecorded -
which is the whole point of sealing instead of hiding.

THE WORKED SHAPE ALREADY EXISTS - COPY IT. Lane route3-2 hit the same problem
in miniature for package LOWER-CERT, whose two files straddle the hook
(lower-cert-base.f must load pre-hook because it arms the certificate
dispatcher the checker calls at every publish; layout-valid.f holds the full
producer and moves post-hook). The fix is src/core/lower-cert-effects.f in
commit 1b73ba47 (.jj-ws/habu-trusted): 16 TRUST rows written INSIDE `package
LOWER-CERT`, before `public`. Measured result - bare MAGIC-V, BUF-N@, HEADER-N
and even LOWER-CERT:HEADER-N all answer E-UNDEFINED at top level, while the
package's own checked half compiles against them. Rows inside a package are
package-scoped, so they buy the checker its facts and change nobody's
visibility. That is the containment this dot has to deliver at registry scale.

THREE THINGS THE SEALING WILL HAVE TO SOLVE THAT ONLY SHOWED UP UNDER ROUTE 3:
1. A recorded row outlives `undefine`. Once type-family.f is checked, its
   `defer TDECL-FIELD-CLEANUP-XT ( n -- )` (type-family.f:1337) records a row
   that generated-declaration-protection.f:196's `undefine` does not retire, so
   CHECKER-RESOLVES? answers true for a retired seam
   (test/type-field-owner-suite.f assert 188). That suite's header explains the
   design being lost: the seam is a `defer` and not a PRIM precisely so it can
   be retired. Same shape for TDECL-FIELD-RELEASE-XT, FULL-PRODUCE and
   FULL-PRODUCE-INSTALL. Sealing may dissolve this by scoping the rows; if it
   does not, `undefine` retiring the row is a checker capability to dot.
2. The package lint blocks three unpackaged files that EVERY prefix change must
   touch: habu2.f's label table, tools/boot-pin.f BP-EACH, and
   test/boot-pin-test.f BPT-PFX-ROW#. Control run on the candidate: an inert
   comment on an untouched global line does NOT fire, so these are genuine
   new/changed globals, not the blanket freeze LESSONS.md describes. Either
   pay the packaging cascade here or get a lint-policy ruling first.
3. Two-step landing. The old engine cannot boot the converted tree and the new
   one cannot boot the unconverted tree, so any landing that changes the prefix
   is one atomic commit AND a staged build: reorder + effects files, install,
   then the deletions, install.

DO NOT RE-DERIVE ROUTE 3 FROM SCRATCH. Commit 1b73ba47 in .jj-ws/habu-trusted
is the banked derivation: the block move, the render-side defer wall, the
accessor redesign that removed all 13 raw `ptr a` handouts of checker private
state, the 101 effect rows, the 95 deleted TRUSTED: forwarders, every ordered
manifest mirrored, and the mutation proofs. Most of it survives the reordering;
re-derive it on top of the seal rather than alongside it.

RE-SCOPED AND RULED (2026-08-20, seal-1's measured checkpoint): the seal is
987 globals / 251 external names / 63 consumer files - three ordered dots,
not one lane. RULING: option 1 - real packages with short public tails
(TFAM:N@ shape), per docs/forth.md's own "prefix-style tails are legacy
debt" and the LOWER-CERT precedent; option 2 (owner names chosen to dodge
OWNER-PREFIX?) is a patch dressed as a seal; option 3 (using everywhere)
re-exposes the surface bare. The PILOT decides on evidence before the
rename cascade: type-schema.f first (98 defs, 49 names, 18 consumers,
ZERO forced renames under the existing SCHEMA-REG owner). Key measured
facts for the lanes: today 37 prefix rows publish registry globals (34
PRIM: in checker.f:6175-6568 + 3 sumtype) and route 3 takes that to 987;
the qualified-probe pair is the acceptance discriminator (sealed private:
PKG:NAME E-UNDEFINED even qualified; bare-name refusal cannot distinguish
package-scoped from unrecorded); sumtype.f's declaration grammar
(NEWTYPE/SUMTYPE/PRODUCT/ENUM/DEFLINEAR + CHECKER-DEF* hooks) is
irreducibly global - its exception row CONVERTS to a narrow
GLOBAL-SURFACE? declaration-shape row with hostile fixtures, never
deletes; type-schema.f is on NO exception list so its 98 globals are
frozen today. All of banked 4f1b44b6 survives; checker.f and
internal-word-gate.f re-derive on top. Census scripts in seal-1's
scratchpad. Iteration cost: 14.7s warm snap.

CORRECTIONS FROM THE PILOT'S FALSIFICATION (seal-2, 2026-08-20): (1) census
is 61 public / 37 private / 18 consumers - type-family and sumtype are
ordinary consumers (+13 names); two maki false positives (maki/schema.f owns
its own SCH-N/SCH-CAP). (2) The LOWER-CERT precedent was measured on PRIVATE
words only - "packaging changes nobody's visibility" does NOT hold for
publics: internal-mark.f:83 gates on wid 0, so package publics are never
classified. (3) NAMING RULED: KEEP THE TAILS - mechanically shortening
collapses 61 publics onto 48 tails (13 collisions across the file's three
sub-registries); disambiguation is a design campaign, unreviewable inside a
seal; OWNER-PREFIX? does not fire on SCHEMA-N@ under SCHEMA-REG, so nothing
forces renames. (4) The pilot c65f76cc is BLOCKED on the internal-mark
mechanism fix; sequence = mechanism, pilot, cascade.

PILOT LANDED (master 4370532e) - the cascade's decision inputs, measured:
keep-tails confirmed cost-free (OWNER-PREFIX? never fired); CONSUMERS IMPORT
VIA using PER THE STANDARD'S OWN MUST-RULE, never qualify (qualification
makes you the packaging owner of every unpackaged consumer file - 80 findings
across seven test suites; using is byte-identical engine, cannot reach
privates, cannot execute marked publics); budget PPRIM rows by MEASUREMENT
(pilot: 2 of 61, both pre-existing); sweep every variable/create for
REG-PROTECT before trusting a census (SCH-RBF-P was a one-line SIGSEGV the
census called unremarkable - TF-RBF-DEPTH is the open sibling, own dot);
acceptance must use the discriminating legs (bare E-UNDEFINED + qualified-
private E-UNDEFINED), never "public still rc 70" which is green both sides.
Iteration cost ~14s install, ~4.5min run.f.

TYPE-FAMILY SEALED (seal-4, 2026-08-21) - the second of the three ordered seals
is done, and it moved the numbers the third one has to plan against.

MEASURED SPLIT. 572 globals -> 221 public / 345 private / 6 that could not move.
The census criterion is "referenced from outside the package", and it has THREE
kinds of caller, not one: 46 consumer files elsewhere in the tree, the checker's
own PRIM: rows, and the five inner packages type-family.f itself opens
(TYPE-NAME, TYPE-FIELD, TYPE-FIELD-OWNER, CHECKER-DECL-FRAME, PREFIX-BOUND).
The earlier scout's 572/163/53 counted only the first kind. Packages DO NOT NEST
(habu2.f C-PACKAGE exits $4B), so the inner blocks close TFAM and reopen it and
read the registry through the file's own `using TFAM` - which is why 50 names
with no tree-wide consumer are public. They cost no capability: a colon public
with no axiom is marked DNAME-INT (rc 70) and every public data record carries
REG-PROTECT, so the qualified spelling is refused exactly like the bare one was.

SIX NAMES ARE ENGINE ABI AND NO SEAL CAN MOVE THEM. habu2.f compiles
match/construct/;match through C-FIND-GLOBAL, which ZEROES the open package's
wordlist cells before the lookup, so TFAM-NAME$, TFL-CON-FAM?, TFL-CVAR? and
TFL-MATCH-FAM? must stay global (bootstrap/cg/forth.fs mirrors the same four
spellings byte for byte); the AOT boot seed re-resolves a baked call site only
through a global scope, so TFAM-CTOR-WORD? (called from src/habu/xref.f) and
TF-SHA16-XT (from src/core/type-family-sha.f) must too. Both failure modes were
measured on the candidate, not assumed: the bare name written to fd 2 with rc 70
on every boot, and `hb: AOT call site unresolved` rc 82. THE THIRD SEAL MUST
SWEEP FOR THESE FIRST: rg the engine sources for `s" <name>"` literals matching
the file's globals, and rg test files for names inside child-program strings
(test/prop-test-core.f names eight registry accessors that no code-token census
reports).

WHAT THE SEAL BOUGHT, measured on master before and after through bin/hb --load:
`0 TF-RBF-P !` + a SUMTYPE went from rc 134 SIGSEGV to E-UNDEFINED (the exact
SCH-RBF-P sibling, private now); `99999 SVX-HI !` + a rejected declaration went
from exit 76 `tfam: ctor index retired after its rows went` to E-UNDEFINED
(SVX-HI has two outside readers, so it is public and gained REG-PROTECT, as did
TF-RBF-DEPTH - dot habu-tf-rbf-depth-614c88e0, closed with this).

TFAM IS A RESERVED SYSTEM-PACKAGE NAME, which the seal inherited for free and
sumtype.f will not: `tfam` is in habu2.f KWDATA:RESTAB-BUF, so
C-QUALIFY-SEAL-GUARD refuses `' TFAM:<any tail>` with rc 84 before any lookup.
That is stronger than the schema package's answer and it covers privates too.
It also breaks any fixture that ticks a TFAM tail - the laundered-execute case
in test/internal-word-gate.f had to move to a SCHEMA-REG cell to keep testing
E-EXEC-OPAQUE-XT rather than the seal guard.

CROSSING BINARY. The installed bin/hb cannot boot a tree whose prefix moved
words into a package, and tools/build-fixpoint.f BF-BOOTSTRAP-STAGE hard-codes
bin/hb as its first-generation compiler, so `install` fails the same way.
bin/hb-host (no AOT window) boots the converted tree: `cp bin/hb-host bin/hb`
then `install --force`. ~15s to cross, byte-identical from the second generation.

The whole-file GLOBAL-IMPLEMENTATION? row is GONE and its positive fixture is a
negative; a narrow GLOBAL-SURFACE? row (TFAM-BRIDGE?) admits the six by exact
path, name and definer, mutation-proved on both clauses.

SUMTYPE SEALED (seal-5, 2026-08-21) - the third and last of the three ordered
seals. src/core/sumtype.f is `package TYPE-DECL`.

MEASURED SPLIT. 316 globals -> 45 public / 271 private (the package holds all 316,
seven of them renamed) plus 7 new one-line globals, over
18 consumer files. The census criterion had only TWO kinds of caller here, not
the three type-family needed: tree-wide consumers, and the file's own global
tail. sumtype.f opens no inner packages at all, so the 50-name inner-package
class that surprised seal-4 does not exist in this file. The engine classes the
type-family seal warned about were swept for and came back EMPTY: no `s" <name>"`
literal in src/habu, src/os, src/arch or bootstrap/cg/forth.fs names any of the
file's globals, and no AOT-captured call site does either - the only string hits
anywhere in the tree are the three block openers inside child-program text, and
those stay global regardless.

THE SEVEN GLOBALS ARE LANGUAGE, NOT ENGINE, and that is a different kind of row
from TFAM-BRIDGE?. NEWTYPE, SUMTYPE and PRODUCT are the block openers, and
CHECKER-DEFFAMILY / CHECKER-DEFSUM / CHECKER-DEFSUM-NOEND / CHECKER-DEFPRODUCT
are the same grammar reached with a body text instead of the input stream. Each
is now a one-line entry into a package public (the ENUM-DECL:ED-RUN shape), so
the buffer, plan arena, capture vectors, renderers and transaction context are
all privates. tools/package-diff-lint-core.f TYPE-DECL-GRAMMAR? admits the seven
by exact path + name + definer; the whole-file GLOBAL-IMPLEMENTATION? row is
gone and its positive fixture is a negative. Eight mutations kill it, one per
clause (row-always-false, no-path, no-definer, suffix-path, any-name,
whole-file-restored, opener-drops-PRODUCT, hook-drops-DEFSUM-NOEND).

FOUR THINGS THE NEXT SEAL AND ROUTE 3 HAVE TO KNOW, all measured on the
candidate, none of them visible from the type-family seal:

1. REG-PROTECT HAS A FIXED BUDGET AND IT WAS NEARLY SPENT. src/core/util.f
   REG-PROT-CAP was 64 and 47 slots were already taken; this seal needs 14 more
   and the live count is now 61. The cap is raised to 192 here. Any later seal
   that publishes data or `defer` records must check REG-PROT-N first - the
   overflow is a die, not a lint.

2. REG-PROTECT CLOSES INTERPRET AND THE TICK, NOT COMPILE MODE, and that decides
   which public records can carry it. TDECL-EVAL-ARMED and TDECL-PROT-WID-ARMED
   are the only two publics here WITHOUT a REG-PROTECT row, because their writers
   (include.f, aot.f, xref.f and the three stage0 bootstrap-wide fixtures) arm at
   genuine top level: with the row, `-1 TYPE-DECL:TDECL-PROT-WID-ARMED !` in
   test/bootstrap-wide-memory-src.f answered `hb: internal engine word` rc 70 and
   reddened the candidate-validation slice.

3. AN UNCHECKED PREFIX FILE'S COLON PUBLICS ARE CHECKER-UNKNOWN, so a CHECKED
   consumer can only reach a `defer`. Moving the arming into include.f's checked
   TDECL-EVAL-INSTALL answered `undefined word 'TDECL-EVAL-ARMED'` rc 70, and an
   ARM word would have needed a brand-new PPRIM axiom to fix a self-inflicted
   problem. This is the ceiling on what package publics buy a file that has no
   checked half; the lower-cert-effects.f shape is the way past it.

4. `is` AND `undefine` ARE PARSING WORDS AND RESOLVE OUTSIDE using-IMPORTS.
   Measured: bare `is TDECL-EVAL-XT` under `using TYPE-DECL` answers `hb: is: no
   deferred word named TDECL-EVAL-XT`, rc 70. Every defer binding in a consumer
   is written `is TYPE-DECL:<tail>`, and because that CHANGES the definition's
   line, the package lint then reports the enclosing global - which is how three
   stage0 fixtures and src/core/include.f ended up packaged rather than waived.

CRASH CLASS. The REG-PROTECT sweep ran every one of the file's 87 data records
through a hostile store followed by a declaration. Six PTR-VARIABLEs were
one-line SIGSEGVs from ordinary `bin/hb --load` source - TDPLAN-P, TDPLAN-ROW-P,
TDPV-CNT-P, TDPV-CELLS-P, TDPV-OFF-P, TDPV-NODE-P - and five more variables
turned a clean declaration into an exit-76 die. All six crash cells are private
now; `0 TDPLAN-P !` before a SUMTYPE went from rc 134 to E-UNDEFINED, and
test/internal-word-gate.f TYPE-DECL-SEAL-CASES keeps that program.

RESIDUALS, both measured and both bounded by an owning dot rather than waived.
(a) The package-reopen defect still reaches the privates: `package TYPE-DECL /
private / 0 TDPLAN-P !` followed by a declaration is rc 134 again, exactly as
`package SCHEMA-REG` and `package PRIM-LINK` are on master. That is dot
habu-pkg-reopen-reaches-113ecd89. TFAM is closed against it only because `tfam`
is in RESTAB, so the available stronger answer here is a RESTAB row rather than
more sealing - a system-package decision, not a seal one, and deliberately not
taken inside this lane. (b) The two armed flags keep a writable qualified
spelling, so `0 TYPE-DECL:TDECL-EVAL-ARMED !` still turns a declaration into an
rc 76 die; item 3 above is why, and the retirement condition is written beside
the cells.

TICK CLASS. `type-decl` is NOT in habu2.f KWDATA:RESTAB-BUF, so this package
answers the SCHEMA-REG way rather than TFAM's: `' TYPE-DECL:<public>` is
`hb: internal engine word` rc 70 and `' TYPE-DECL:<private>` is E-UNDEFINED,
never rc 84. The gate asserts that answer, so adding the name to RESTAB later is
a visible design change rather than a silent one.

WHAT ROUTE 3 MUST NOW RE-DERIVE DIFFERENTLY. The banked derivation 4f1b44b6
predates all three seals and its exposure table is void: type-schema.f,
type-family.f and sumtype.f no longer contribute 621 globals between them, they
contribute the six TFAM bridges plus these seven grammar names. What survives is
the block move, the render-side defer wall, the accessor redesign, the 101
effect rows and the 95 deleted TRUSTED: forwarders. What has to be re-derived on
top of the seals: (a) every PRIM: row for a name that is now a package public is
a PPRIM: <OWNER> row, and checker.f already carries 24 PPRIM: TFAM and 2 PPRIM:
SCHEMA-REG rows as the worked shape; (b) the 662 definitions route 3 wanted to
publish are mostly PRIVATE now, so checking them publishes nothing - the
exposure that stopped the lane is gone rather than mitigated; (c) sumtype.f is
still loaded UNCHECKED, so route 3's own goal for this file runs into item 3
above and needs the checked-half split before its publics are callable from
checked code; (d) layout-buffer.f (32 globals) is the one row of the original
exposure table nobody has sealed, and it is now the only unsealed registry.
