---
title: Migrate engine hooks to typed xt cells
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-16T08:27:22.049121+02:00\""
---

Stage 2 of the stored-xt soundness program (RCA + sequencing in habu-checker-exec-of-5923c543; stage 1 landed: typed xt<effect> cells, f369f37c). Migrate the ~36 engine hook cells that use the raw-var @ execute shape - the RCA's measured inventory: checker.f/type-family.f/sumtype.f/layout-buffer.f plugin dispatch (tfam-resolve*/arity*/layout?*/cell?*/width@*/concrete-linear?, match-fam-tok/match-variant-tok/match-of-tok/construct-tok, checker-usig-cert-add/checker-package/checker-undefine-guard/checker-lbuf-name-guard/checker-snapshot-prepare, rbf-push/rbf-pop/report-uncheckable/usig-add-bad/sig-type/loc-show-one/check, lbuf-eval-run/layout-maybe-linear?/layout-linear-count, tdecl-gen-eval/tdecl-ctor-prot-wid/tf-ctor-build-hash) and habu1.f hooks (source-hook/prefix-hook/cold-hook/restore-hook/proof-hook) - to typed xt<E> cells (TYPED-VARIABLE with the hook's true effect) or defer/is where install semantics fit; fprim/fprim-l/fprim-wid stay TRUSTED machine-code boundaries (classify explicitly). Each migrated hook: declared effect matches the real callers (derive from the current install sites), store sites become first-class quotation stores or typed installs, fetch+execute fit-checks. Acceptance: zero raw-var @ execute sites remain in the inventory (rg proof), all suites + self-check certify (Certified count may move - STATUS.md updated honestly), fixpoint x2, full run.f, maki 0 FAIL; the stage-3 pin (xt-cell-test L1) still shows plain user variable laundering (the flip is stage 3, not this dot). COORDINATE: broad checker.f surface - tfam's sealed-packages lane is active; keep hunks per-hook surgical; integrator rebases by hash. Files: src/core/checker.f, type-family.f, sumtype.f, layout-buffer.f, render.f, src/habu/habu1.f, STATUS.md, tests. Ownership: checker/engine hook typing.


RE-SCOPED 2026-07-16 (hookmig lane; enabler landed 7e93b439 "engine: load
exec-vector before checker prefix" - the prefix order lives in SEVEN
synchronized mirrors + pinned codegen-test counts, all updated). The migration
itself was BUILT AND NATIVE-GREEN (defer conversions + default-is guard
restructuring + installer words; fixpoint x2; full run.f correctness-green)
then honestly REVERTED on two proven blockers:

- BLOCKER 1 (hard): RESOLVED 2026-07-16 - habu-mirror-defer-is-4461fe23 landed
  (stage0 forth.fs now mirrors C-DEFER/J-IS; wide-memory defer/is round-trip
  green; full recovery CHECK_ONLY chain OK). Scope boundary that constrains
  stage 2: the stage0 defer/is works on the UNCHECKED load path only (files
  before check-hook INSTALL, TRUSTED: bodies); a defer/is inside a CHECKED
  definition in a stage0-loaded prefix file is rejected fail-closed because the
  mirror lacks the C-CALL-CHECKER-DEFER registration bridge. Either keep
  stage-2 defer shapes in unchecked-region files, or land
  habu-mirror-checker-defer-6a8a366e first.
- BLOCKER 2 (ordering): checker.f-internal hooks (rbf-push/rbf-pop/sig-type/
  check/...) are installed BEFORE the checker prefix is live, so TYPED-VARIABLE
  xt<E> cells cannot type their own bootstrap installs without a trust seam;
  the enabler (exec-vector before checker prefix) fixes the habu1.f hook class
  only. The checker-internal class needs either stage0 defer/is (blocker 1) or
  a typed pre-prefix install primitive - decide when blocker 1 lands.

Per-hook inventory is the measured list in this dot's opening paragraph; the
per-hook effects were derived once by the hookmig lane (workspace retired,
revert not preserved) and are re-derivable from the current install sites as
the acceptance text specifies.
Claim released (agent=hookmig done; enabler integrated by orchestrator).
Blocked-by: none (habu-mirror-defer-is-4461fe23 landed and closed 2026-07-16);
blocker 2 (install-ordering) remains a design constraint documented above.