---
title: "TFAM 2b: sealed system packages + friend latch + provenance"
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-03T23:36:48.915549+02:00\""
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
