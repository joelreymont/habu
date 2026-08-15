---
title: Seed call-site resolution is wordlist-blind
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-15T23:39:19.899983+02:00\""
---

Claim: agent=bake-chain-9 workspace=.jj-ws/habu-bake-chain

P1 found by bake-chain-8 (2026-08-15), debugger-proven not inferred: EM-AOT-PATCH-SITES (habu2.f:4401 pnf) resolves each captured call site's BARE name via LFIND in the global wordlist; a packaged callee never resolves and the exit is a SILENT 81 ($51, no message - distinct from layout.f BL-RANGE-RC 81 which writes one). Caught at 0x10000c588: x13=0 LFIND miss, site 219, pool entry CDIGEST:SLOT@ - public in package CDIGEST (src/compiler/digest.f:86). Measured over the merged buffers: 18896 sites = 15421 packaged callees + 3475 prefix words - 82% of the chain cannot be relocated today. Never fired because the only window ever captured (the REPL) defines no package; corroboration: aot-wid-build's gate modes already had to put QUALIFIED names on the boot-run list. Qualifying names is refuted as the fix: it reaches only PUBLIC package words and the chain has private callees in quantity. RULED fix direction: each site carries its callee's WID beside the name - window-relative, rebased by exactly the record machinery (54dec421's discipline), site row widens 8B->12B (one site = one row; the existing parallel tables are value families, not fields of one record), artifact VERSION 3, seed resolves wordlist-aware, and the capture-side audit that should have caught this (every site's name resolves THE WAY THE SEED WILL ASK) lands with the fix. Acceptance: the merged-engine boot milestone (NMIGRATE:DEFINE at first user token) plus a minimal packaged-callee fixture window (a package word calling another package's public AND private word), mutation-proven; the silent $51 exit gains a named diagnostic either way. Blocks e98b03d4 items (3)-(6).

RULINGS 2026-08-16 (bake-chain-9 checkpoint; the census - 3452
global, 4424 in-window public, 10994 in-window private, 12 wid-2,
11 pre-window dynamic, 14772 sealed-callee - is the evidence base):
D1: FOUR clauses, as measured. [0, FIRST-DYNAMIC-WID) are layout
CONSTANTS and pass through (the wid-0 argument extends to them);
window coordinates rebase; PRE-WINDOW DYNAMIC wids take option
(a) - the capture stores the QUALIFIED name plus a QUALIFIED
marker in the wid field, the seed resolves through LFIND's
existing qualifier path; a pre-window PRIVATE callee is refused
by name (structurally empty today - a caller in P's private
scope is itself a P record, which ACAP-?WID refuses). Option (b)
verbatim wid is REFUSED - it rests on a prefix-identity
invariant nothing enforces. Anything else: refuse by name.
D2: factor search-wl/BSWL's body into ONE labelled routine
called by both the primitive and the seed - approved. BSWL keeps
its wid-2 short-circuit at the primitive boundary; the seed's
routine searches wid 2. The capture-side audit covers 18881
sites through search-wl and carries the 12 wid-2 sites by the
STRUCTURAL check (callee record is pre-window and its wid is the
layout constant). A new primitive is REFUSED - it would leak the
sealed helper wordlist to checked habu, and the seal threat
model is checked habu.
D3: the call-site patch pass exempts exactly the REBASED
IN-WINDOW class - callees whose resolved wid lies in [T0,
T0+span), wordlists the seed itself just created, never the
engine's pre-existing sealed prefix. The gate's purpose
(protect pre-existing sealed wordlists from baked injection) is
untouched; the exemption is keyed on the rebase classification,
not on ordering. Moving SEAL-WIDS after the patch pass is
REFUSED (implicit ordering invariant); refusing the chain is
REFUSED (no milestone). Pre-window callees, code literals, and
boot-run names keep the gate unchanged - aot-wid-build's gate
modes keep their contract exactly.
NOTED: site rows 12B force AOT-SECTION-CAP $500000->$520000 and
CODE-CAP-BYTES $A00000->$A20000 - the AGREE chain guards the
lift; if MACHO-MSIZE-CHECK trips, re-derive MSIZE by
image-bytes.f's own method (the fourth-coupled-term lesson).
Seed-affecting: install --force before every cold gate.
LANDED IN PART 2026-08-16 (bake-chain-9, commit on
.jj-ws/habu-bake-chain over master dc18bbca). The four-clause
scope is IN and the P1 is fixed: a site row is 12B (blob-off,
name-off, scope), artifact VERSION 3, the seed resolves through
`search-wl`'s own body - factored into ONE labelled routine
(habu1.f WLFIND:EMIT / WLFIND:LENTRY, x0/x1/x2 in, x11 = xt,
x12 = row) that the primitive and the seed both BL. BSWL keeps
the wid-2 short-circuit; the seed's calls do not. The silent $51
is gone: two named fd-2 diagnostics ("hb: AOT call site
unresolved", "... names a wordlist outside the window"), both
exit ENGINE-ERROR:AOT-SEED like the sibling passes. The capture
audit ACAP-?SITE reads the ROW just written and requires the
engine's own find to answer the callee's xt, over all 18893
sites; the 12 OWNER-API-PRI-WID sites take the structural check.
MEASURED END TO END: the metabuild merges the real 3.1 MB chain
artifact and emits an engine; 18893 sites relocate; that engine
boots, runs ordinary programs, and the chain's own boot-run
installer resolves and runs.
TWO GATE RULINGS ARE OWED BEFORE THE MILESTONE CLOSES, both
measured, both outside D3 as written:
(1) 3 call sites resolve into a PRE-WINDOW protected wordlist -
CODE-RECLAIM:WATCH, a PUBLIC word of a sealed package the chain
calls from checked source. [T0,T0+span) cannot cover it: the
seed did not create that wordlist.
(2) the chain's boot-run entry A64RAV:DKEEP-HOOK-DEFAULT is a
public word of an IN-WINDOW protected package
(regalloc-verify.f:1886 `get-current prot-wid-add`), and D3 kept
the gate for boot-run names.
Both die exit 84 "hb: AOT protected-WID gate reject". THE LIVE
SEAL DOES NOT FORBID EITHER: habu2.f EMIT-STORE-DEF-NAME refuses
DEFINING into a protected wid and C-PACKAGE-SEAL-GUARD refuses
OPENING one - calling a public word of a sealed package is what
checked source does today. Proposal: the gate admits a callee
that is PUBLIC in its package (the resolved wid is some package
row's [0]) and refuses a private one, on all three paths;
aot-wid-build's gate modes then need their protected package to
be a private-reaching case. Alternative: extend [T0,T0+span) to
the boot-run path (fixes (2) only).
AND ONE NEW DEFECT, not this dot's: with both gate rejections
bypassed the merged engine boots and `NMIGRATE:DEFINE` CRASHES
(SIGSEGV, exit 134, pc 0x10052191c) where the same call on the
source-loaded chain returns 0. Without the chain's boot-run
installer it throws 7134 = E-PATH-RANGE (src/core/util.f PATHZ)
instead. Ordinary programs run fine in the same engine. That is
the next blocker for e98b03d4's milestone and wants its own dot
and a debugger session.
VEHICLE: tools/aot-chain-bake.f (new) splices
STDIN-DRIVER:ARTIFACT! into stdin.f and runs the production
stdin build - `bin/hb --load tools/aot-chain-bake.f -- <artifact>
<producer-engine>` leaves $HB_TMP/hb-chain. It has no suite yet
(the milestone it would assert is blocked); the schedule lint is
satisfied because it is a build tool beside
tools/aot-chain-capture.f, not a test.
MUTATIONS PROVEN: capture scope forced to 0 for a packaged
callee (the original defect) -> the audit refuses naming SLOT@;
the qualified spelling dropped -> the audit refuses naming
BYTES-ALLOC-LEN in scope 4294967294; the merge's scope rebase
deleted -> 185 out-of-range coordinates; the QUALIFIED marker
treated as movable -> the merge refuses by name; a wrong shift
quantity, a uniform off-by-one and a single-row off-by-one ->
all refused. For this family the RANGE check fires first on
every mutation, so the sum-per-row-family check is present and
consistent with the file's other ten families but has no kill of
its own.


GATE DISCRIMINATOR RULING 2026-08-16 (bake-chain-9 landing): the
sealed-WID gate's stated purpose was broader than the invariant
the live seal enforces - EMIT-STORE-DEF-NAME refuses DEFINING
into a protected wid, C-PACKAGE-SEAL-GUARD refuses OPENING one,
and CALLING a public word of a sealed package is what checked
source does every day. RULED: the gate admits a callee that is
PUBLIC in its package and refuses a PRIVATE one, on all three
paths (call sites, code literals, boot-run names) - aligning the
gate with the seal's real discriminator instead of the over-broad
"any protected wid". This covers both measured refusals (the 3
pre-window CODE-RECLAIM:WATCH sites and the in-window
DKEEP-HOOK-DEFAULT boot-run entry) WITHOUT exemption classes.
test/aot-wid-build.f's gate modes convert to a PRIVATE-reaching
case so EM-AOTWIDGATE keeps a live red path - the gate must not
go quiet. The D3 in-window exemption is SUPERSEDED by this
simpler rule (delete it if it landed; public/private is the
whole discriminator). Mutation: a private callee of a sealed
package must still refuse on every path.
The clobber-lint note in habu1.f ("the stale rows cost nothing
today") is one read from false after WLFIND - annotate via
305ed456's leaf, not by editing the note in place.

RESOLUTION LANDED 2026-08-16 (bake-chain-9, merged 88e31d8d):
12B site rows with the four-clause scope, one WLFIND routine
shared by search-wl and the seed, named diagnostics replacing
the silent $51, the resolves-as-the-seed-asks audit over all
18893 sites, VERSION 3, caps lifted ($520000/$A20000/MSIZE
$A50000 re-derived). 18893 sites relocate; the merged engine
boots and runs programs and the boot-run installer fires.
MILESTONE STILL OPEN behind: (a) the GATE DISCRIMINATOR RULING
above (public-callable/private-refused - supersedes D3's
exemption); (b) the merged-engine NMIGRATE:DEFINE crash, dotted
c970bf04. This dot stays active until the gate ruling lands and
the milestone transcript is green.

