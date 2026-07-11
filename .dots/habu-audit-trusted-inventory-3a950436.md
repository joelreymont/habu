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

## Increment 2026-07-11 (row granularity complete; fold ratchet)

AUDIT (from the live TSV, 659 sites): 47 placeholder folds covered 510 sites;
15 more folds with real owners covered ~70. Separability: 55 folds were fully
nameable; 7 folds each held exactly one unnameable `0 set-check` site; zero
stale rows (strict was green: no dead/unmatched rows existed).

DONE: all 59 separable folds (except the two contested files) split into 427
`file:name` rows — class and owner UNCHANGED per row (granularity only; no
owner guessing). The 7 set-check files keep count-1 residual file rows.
Duplicate site names carry explicit counts (test/engine-suite.f:T-RDF 2).
Trust surface identical: same 659 sites, same classes per file (by-file lines
unchanged), strict + derived ratchet + baseline mode green.

SKIPPED BY DESIGN: `src/habu/habu2.f` (122 sites) and
`test/type-layout-lower-pending.f` (4) — contested under the wide-ADT stack;
their per-name rows would go stale on that merge. Split them when ownership
releases, lowering the fold-baseline in the same change.

RATCHET: tools/trusted-inventory.f strict now computes the separable-fold
count (a file-level row whose matched sites are all nameable), prints
`separable fold(s) N (baseline M)`, and fails when N exceeds the committed
`fold-baseline` directive (TRUSTED.md block head, currently 2); a missing
directive is a strict failure. Red-first proven: re-folding a split file ->
rc=81 with per-fold detail; deleting the directive -> rc=81 named failure.
CMAX 512 -> 1024 (block now ~525 rows); CTAB gains K-UNNAME.

REMAINING (ownership, unchanged scope): 409 placeholder-owned rows now at
word granularity await per-site owner reassignment (builder-emit ~210 named +
habu2 fold, test-metaprog ~95 named + residuals, stdlib-boundary PTX/engine-id
~71 named, discharge-candidate 4) — per-site domain judgment, further bounded
increments per the rules above.

## Increment 2026-07-11b (discharge-candidate class resolved)

The 4 `src/core/combinators.f` rows (TIMES/EACH/MAP/FOLD, combinators.f:20-34)
are NOT dischargeable today: each re-executes a stored quotation per loop
iteration (`r@ execute` / local-`q execute` inside `?do`), which types only
under the multishot-quotation capability — the file's own boundary comment
says exactly this and names the owner. Reassigned to
`habu-multishot-quotations-typed-8832cace` (whose text lists these words) and
re-classed `discharge-candidate` -> `stdlib-boundary` (the class definition is
"believed checkable today", which the evidence contradicts; the tile-rows
precedent classes capability-blocked library boundaries as stdlib-boundary
owned by the capability dot). BI/TRI in the same file are already plain
checked definitions — no rows. Zero placeholder discharge-candidate rows
remain.
