---
title: Migrate engine hooks to typed xt cells
status: active
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-16T08:27:22.049121+02:00\\\"\""
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
STAGE-2A LANDED 2026-07-16 (stage2a lane; claim released): all five habu1.f
hooks (source/prefix/cold/restore/proof) converted from raw-var @ execute to
defer/is with declared effects and preserved default semantics (explicit
default-is: SOURCE-DEFAULT for source-hook, HOOK-NOOP for the 0-guard class;
habu2's restore install is a colon-wrapped typed quotation since [: is
compile-only). rg proof: zero *-XT sites remain in habu1.f (FP-XT/fprim
excluded by scope - already checked since the btrust batch). Stage-3 pin
intact (xt-cell-test L1 still launders). Proven on BOTH paths after the
checker-defer bridge: native fixpoint x2 0a71f23d; full recovery chain
'bootstrap check OK' with the mirror-built seed certifying the checked
defer/is hooks. Zero new TRUSTED sites; owner-wid-emit-seal absence rows
follow the new words.

REMAINING = STAGE-2B (checker.f/type-family/sumtype/layout-buffer/render
hooks, the ~31-cell balance of the opening inventory): the bridge removed the
mirror-side rejection; the open constraints are (1) the install-ordering
design - those hooks install BEFORE the checker prefix is live, so either
stage0-compatible defer placement or a typed pre-prefix install primitive,
decide at dispatch; (2) tfam sealed-packages coordination on checker.f hunks
(surgical per-hook hunks, integrator rebases by hash). Corrected premise for
2b (proven by stage-2a): the engine prefix files are in the CHECKED region of
the generated stage sources - 2b conversions will be checker-certified code,
not unchecked-region code.


STAGE-2A STATUS 2026-07-16 (stage2a lane, implementation COMPLETE and
native-green, held uncommitted in .jj-ws/fable-stage2a pending the bridge):
PREMISE CORRECTION - the habu1.f hooks are NOT unchecked-region code: in the
assembled stage2-src, LOWER-CERT-HOOK:INSTALL (checking ON) precedes
SOURCE-HOOK! with no 0 set-check between, so the owner-wid hooks compile
CHECKED. Native certifies checked defer+is fine (fixpoint x2 5a7638a6);
the gforth-mirror stage0 rejects it - exit 70 'hook: non-certified definition:
source-hook! at is' - because mirror C-DEFER (forth.fs:4344) omits
C-CALL-CHECKER-DEFER. Option A (unchecked-region placement) would require
adding a forbidden 0 set-check boundary; therefore ALL of stage 2 gates on
habu-mirror-checker-defer-6a8a366e (blocks: edge added). Minimal blocker
fixture: any 'defer FOO (E)' + ': FOO! ([E] --) is FOO ;' after
LOWER-CERT-HOOK:INSTALL on the mirror-built stage. Per-hook conversion table,
rg proof (zero raw *-XT sites), stage-3 pin intact, and full gate tails are in
the stage2a lane report. When the bridge lands: rerun recovery in the
workspace, commit 'engine: habu1 hooks to defer/is (stage 2a)', integrate.

BRIDGE LANDED 2026-07-16 (habu-mirror-checker-defer-6a8a366e closed, 5d2f6d29):
the mirror-built seed now certifies checked defer/is. Stage-2a proceeds from
the held implementation in .jj-ws/fable-stage2a (rerun recovery, commit,
integrate). Stage-2b's defer approach is likewise unblocked on the stage0 side;
its remaining constraint is the checker.f-internal install-ordering design +
tfam coordination.
