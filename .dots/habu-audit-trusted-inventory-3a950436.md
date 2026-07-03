---
title: Audit trusted-inventory classification to row granularity
status: open
priority: 2
issue-type: task
created-at: "2026-07-02T10:30:00.000000+02:00"
---

TRUSTED.md's trusted-inventory-classes block is hand-curated at FILE granularity: one `file class dot` row classifies every trust site in that file, and this dot is the placeholder owner of ~58 rows. Refine it to row granularity: (1) replace each file-level row with `file:name` rows carrying the honest per-site class (builder-emit, stdlib-boundary, test-metaprog, prim-axiom, discharge-candidate), keeping a file-level row only where every site in the file genuinely shares class and owner; (2) reassign ownership from this audit dot to the real capability/discharge dot for each site - discharge-candidate sites each need a discharge dot, checker-capability sites the matching capability dot (hook installs are already row-granular under habu-police-set-check-850bc543); (3) keep `bin/hb --load tools/trusted-inventory.f -- strict` green throughout - it fails on unclassified sites and on owning dots missing from .dots/. Done when no row's owner is this dot.

## Progress (partial)

Row-granularity refinement started on the two big uniform files:
- src/core/roles.f: the single file-level row is replaced by 34 `file:name`
  prim-axiom rows (>IDX/IDX>N ... >SNAP/SNAP>N), one per nominal-cast site.
- test/prop-test-core.f: 15 `file:name` test-metaprog rows for its TRUSTED
  fixtures. The file-level row is retained because the two `0 set-check`
  boundaries have a space in their site name and cannot be a `file:name` key
  (CROW-PARSE splits the row on whitespace); PROP-CHECK-HOOK stays under
  habu-police-set-check-850bc543.

tools/trusted-inventory.f strict mode now emits a `by-file` line per source with
its non-zero per-class site counts (CLASS-BY-FILE-REPORT), covered by FIX-BY-FILE
in tools/trusted-inventory-test.f. Ratchet counts are unchanged.

Increment (prim-axiom class fully re-owned): all 37 `prim-axiom` rows that sat on
this placeholder — the 34 `src/core/roles.f` nominal-cast converters plus the
engine-primitive TRUST rows in `src/core/structures-effects.f` (CELL/+FIELD/…),
`tools/check-core.f` (CHECKER-DEFTYPE/SCOPE/…), and `src/core/include.f`
(INCLUDE-MMAP-PTR, INCLUDE-EVALUATE) — are reassigned to their real owner
`habu-primitive-effect-axiom-1119f176` (the audited axiom table whose mandate is
exactly this class; the 5 prop-test-core AX-* rows already lived there). The
whole `prim-axiom` class (42 rows) is now off the placeholder. Evidence: each
reassigned site is an engine-primitive TRUST row / nominal identity cast the
checker treats as an axiom, i.e. axiom-table scope, not a discharge candidate.
`strict`, the derived ratchet, and the full gate stay green.

Dot stays OPEN: 68 placeholder rows remain (`builder-emit` 34, `test-metaprog`
23, `stdlib-boundary` 10, `discharge-candidate` 1). Its done-criterion ("no row's
owner is this dot") is not met until each is reassigned to a real owner, which
needs per-site domain judgment and, for several classes, a correctly-scoped
capability/discharge owner (builder emitters vs raw-layout axioms differ;
`habu-builder-trust-rows-c5d41af6` owns the dischargeable builder emit effects,
`habu-checker-capability-typed-e0c76a02` the ptx tile sites,
`habu-typed-depth-introspection-18f0efda` the depth-capture test-metaprog class).
Do these as further bounded, evidenced increments — do not bulk-guess owners.
