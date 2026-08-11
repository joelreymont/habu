---
title: Turn the registry bridges into checker axioms
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T06:55:29.416999+02:00"
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
