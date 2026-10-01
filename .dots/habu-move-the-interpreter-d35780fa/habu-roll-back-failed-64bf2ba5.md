---
title: Roll back failed definitions in Habu
status: open
priority: 2
issue-type: task
created-at: "2026-09-29T13:12:28.948447+03:00"
blocks:
  - habu-move-evaluate-and-9119f746
  - habu-compile-definer-bodies-1292d049
---

Problem: failed-definition rollback is the assembly `LEVALREC`.
Acceptance: a failed definition rolls back dictionary, DP, code cursor, address rows and package scope (`EM-PKG-RESYNC`) exactly as `LEVALREC` does; the `repl-address-cell-rollback` and `load-reject-diag` suites green through the Habu loop under the feature cell.
Files: `src/habu/outer.f`.
Verify: spark: the `repl-address-cell-rollback` and `load-reject-diag` suites; gate.
Depends: habu-move-evaluate-and-9119f746 (I9a), habu-compile-definer-bodies-1292d049 (I7).
Route: Alder (shared: src/habu/outer.f).
Ownership: krait (Intel lane).
Claim: unassigned.

Lead note (2026-10-01, I5e habu-close-definitions-with-8ace78d8 landed): a failed `;` in the Habu loop keeps PEND and the open provenance window, as the engine's `;` does, and the engine's LEVALREC rolls both back today; this leaf's rollback clears them. Visible only through a catch around an included file or an exit hook; untested.
