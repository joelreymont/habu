---
title: Migrate engine hooks to typed xt cells
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-16T08:27:22.049121+02:00\""
---

Stage 2 of the stored-xt soundness program (RCA + sequencing in habu-checker-exec-of-5923c543; stage 1 landed: typed xt<effect> cells, f369f37c). Migrate the ~36 engine hook cells that use the raw-var @ execute shape - the RCA's measured inventory: checker.f/type-family.f/sumtype.f/layout-buffer.f plugin dispatch (tfam-resolve*/arity*/layout?*/cell?*/width@*/concrete-linear?, match-fam-tok/match-variant-tok/match-of-tok/construct-tok, checker-usig-cert-add/checker-package/checker-undefine-guard/checker-lbuf-name-guard/checker-snapshot-prepare, rbf-push/rbf-pop/report-uncheckable/usig-add-bad/sig-type/loc-show-one/check, lbuf-eval-run/layout-maybe-linear?/layout-linear-count, tdecl-gen-eval/tdecl-ctor-prot-wid/tf-ctor-build-hash) and habu1.f hooks (source-hook/prefix-hook/cold-hook/restore-hook/proof-hook) - to typed xt<E> cells (TYPED-VARIABLE with the hook's true effect) or defer/is where install semantics fit; fprim/fprim-l/fprim-wid stay TRUSTED machine-code boundaries (classify explicitly). Each migrated hook: declared effect matches the real callers (derive from the current install sites), store sites become first-class quotation stores or typed installs, fetch+execute fit-checks. Acceptance: zero raw-var @ execute sites remain in the inventory (rg proof), all suites + self-check certify (Certified count may move - STATUS.md updated honestly), fixpoint x2, full run.f, maki 0 FAIL; the stage-3 pin (xt-cell-test L1) still shows plain user variable laundering (the flip is stage 3, not this dot). COORDINATE: broad checker.f surface - tfam's sealed-packages lane is active; keep hunks per-hook surgical; integrator rebases by hash. Files: src/core/checker.f, type-family.f, sumtype.f, layout-buffer.f, render.f, src/habu/habu1.f, STATUS.md, tests. Ownership: checker/engine hook typing.

Claim: agent=hookmig workspace=.jj-ws/fable-hookmig
