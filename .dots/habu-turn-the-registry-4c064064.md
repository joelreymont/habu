---
title: Turn the registry bridges into checker axioms
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-10T06:55:29.416999+02:00\""
---

src/compiler/native/family.f carries ten one-line TRUSTED: bridges onto the type-family registry (TFL-MATCH-FAM? etc.) because the registry's readers live in the boot prefix where the seal strips their symbols - the same wall dict.f met. The structural replacement is one PRIM: row per word in src/core/checker.f, which is a boot-prefix change under the two-stage rule plus the bootstrap/cg/forth.fs mirror. Do dict.f's boundary in the same pass. Acceptance: the TRUSTED: rows in family.f and dict.f deleted; the PRIM rows assert the same effects; bootstrap mirror lint green; two-stage landing per docs/bootstrap.md. Files: src/core/checker.f, bootstrap/cg/forth.fs, src/compiler/native/{family,dict}.f. Depends: none (schedule with any other boot-prefix change to amortise the two-stage cost).

ESCALATED AND WIDENED 2026-08-11 (user directive: the tree's standard is
PRIM, not TRUSTED). Scope is now EVERY TRUSTED: site in the chain, not just
family.f/dict.f - inventory on master 077e69a3: family.f 10 (registry
bridges), dict.f 12 (RUN-WORD, EFF-CELLS, EFF-DEAD?, the five quotation
readers, EFF-QUERY/COUNTS, the two slot readers, TRAILER@), publish.f 5
(CODE-WINDOW, RELOC-EXTERNAL, RELOC-ADDR, RETARGET-REC, MIN-IN-REC), migrate.f 1 (EV),
reach.f 2 (POKE, EFFECT), string.f 1 (PTR>N - its retirement owner note
stays valid). Campaign-added sites cited the dict.f precedent each time -
the precedent must die in one landing. ONE boot-prefix pass: a PRIM: row per
word in src/core/checker.f asserting the effect the underlying definition
declares, the bootstrap/cg/forth.fs mirror question re-verified (the pkgasm
lane proved the mirror reads package declarations; PRIM rows are its bread),
two-stage per docs/bootstrap.md, every TRUSTED: deleted with the package
lint proving none returns. The new addrmap-set already landed as PRIM - the
worked example. PAIR WITH the fail-closed TRUST fix (see
habu-make-trust-fail dot): converting rows while TRUST silently mints
symbols for typos would hide mistakes the conversion makes.

INVENTORY UPDATE 2026-08-11 (aotsite lane): publish.f is now FIVE sites, not
four - RELOC-ADDR joined it with the per-site address record. It wraps
`addrmap-set`, which is PRIM-TRUSTED-ONLY! for the same reason `callmap-set`
is, so a checked caller earns E-CAP-TRUSTED at the call and TRUSTED: is the
only route the checker admits today. It is one more instance of the pattern
this dot retires, not a new pattern, and it converts with the other four in
the one boot-prefix landing.

Claim: agent=primsweep workspace=.jj-ws/habu-prim-sweep

MEASURED CLASSIFICATION 2026-08-11 (primsweep lane). The 31 sites are four
classes, not one, and only 21 can become PRIM rows. What decides it is what the
TRUSTED: body wraps.

Class A - a boot prefix HABU word the seal marks internal, so the checker has
no symbol for it and a checked caller is E-UNDEFINED. 21 sites: family.f 10
(the whole file), dict.f 10 (EFF-CELLS, EFF-DEAD?, the four quotation readers,
EFF-QUERY, EFF-COUNTS, the two slot readers), reach.f EFFECT. These convert.
The mechanism is not new: src/core/checker.f already carries PRIM: rows for two
boot prefix colon words - EXT-MARK-FREE-TAIL (checker.f itself) and
CHECKER-DEFFAMILY (sumtype.f) - and the row beside EXT-MARK-FREE-TAIL states
why: the axiom keeps the word checker-known so the seal-time internal-word
marking pass leaves it callable. A row interns a symbol and stores an effect;
it never required the named word to be an engine primitive.

Class B - the wrapped word ALREADY has a PRIM: row, deliberately marked
PRIM-TRUSTED-ONLY! so a checked caller earns E-CAP-TRUSTED. 6 sites: publish.f
CODE-WINDOW (code-publish), RELOC-EXTERNAL (callmap-set), RELOC-ADDR
(addrmap-set), RETARGET-REC (xref-retarget), MIN-IN-REC (min-in-mark, which
would need the same flag), reach.f POKE (patch32). Converting one means
DELETING the flag - opening code injection and relocation metadata to every
checked caller. That is not crossing the boundary, it is removing it, and it
was rejected once already. These stay named boundaries.

Class B-prime - migrate.f EV (evaluate). A row for `evaluate` would not restore
a reachable word's declared effect; it would hand every checked body the
compile-arbitrary-source door. Stays.

Class C/D - no underlying word to name, or an operation with no expressible
type. dict.f TRAILER@ (a raw cell fetch at a bare n), string.f PTR>N (an
identity cast with an EMPTY body - there is nothing for a row to name), dict.f
RUN-WORD (execute). Owners already recorded in the files:
habu-typed-xt-storage-ddad4af8 and habu-guard-an-executed-8a0f2f77.

LANDED (Class A, 21 of 31): 22 PRIM: rows in src/core/checker.f (ten registry
readers, eleven effect-store readers, and CTL-DEAD?); the ten family.f bridges
deleted and its public words calling the registry directly; dict.f's eight pure
renames deleted with their call sites naming the reader, EFF-CELLS and
EFF-COUNTS now checked `:`; reach.f EFFECT now a checked `:`. One duplicate
disappeared with them: dict.f held its own copy of the checker's `CTL-DEAD and
0 <>` mask, and the new checker word CTL-DEAD? answers the question so the mask
lives once, with the encoding it reads.

NO MIRROR CHANGE AND NO TWO-STAGE LANDING, measured rather than assumed.
bootstrap/cg/forth.fs carries the prefix FILE LIST (`s" src/core/checker.f"
PFX-LOAD-ROW`), not checker.f's contents, and holds no PRIM:/PE-IN/PTABLE text
at all. checker.f is read from the checkout at boot, so rows take effect with no
rebuild. LESSONS' two-stage rule governs a NEW ENGINE PRIMITIVE called from the
prefix and explicitly tolerates a `PRIM: NAME PRIM;` axiom line.

FAIL-CLOSED, measured by mutation: a missing or wrong row does not degrade
quietly, it stops the owning file from loading, naming the word at the caller.
Deleting the TFAM-SLOTS@ row gives `E-UNDEFINED ... in width: undefined word
'TFAM-SLOTS@'`; deleting EFFECT-QUERY's gives the same at dict.f's eff-cells;
giving EFFECT-DIN-N a wrong arity reds reach.f at eff-counts. So every suite
that loads the native chain is the net for a conversion mistake.

REMAINING: the 10 sites above stay as named boundaries with the owners named
in their own files. This dot covers Class A only; nothing further is owed here.
