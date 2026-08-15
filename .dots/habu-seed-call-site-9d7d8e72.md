---
title: Seed call-site resolution is wordlist-blind
status: open
priority: 2
issue-type: task
created-at: "2026-08-15T23:39:19.899983+02:00"
---

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
