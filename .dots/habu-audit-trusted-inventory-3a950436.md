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

Dot stays OPEN: it is still the owning dot of the remaining file-level rows and of
the roles.f/prop-test-core.f rows above, so archiving it would turn
`trusted-inventory.f -- strict` red (DOT-EXISTS?). Its own done-criterion ("no
row's owner is this dot") is not met until every remaining row is reassigned to a
real discharge/capability owner, which is out of this worker's scope. Remaining
work: refine the other file-level rows and reassign owners.
