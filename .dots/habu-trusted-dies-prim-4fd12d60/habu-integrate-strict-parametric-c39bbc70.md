---
title: Integrate strict parametric effects and checked Forth helpers
status: closed
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.337312+03:00"
closed-at: "2026-09-16T14:34:48.737785+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Strict parametric effects are in force but five suites are still red under them and docs/forth.md does not state the rule"
---

Owner: Cedar; nominal_pointer handles the remaining reduced compiler binding failure. Strict nominal/generic pointers and type-variable kinds are integrated. Concrete byte/cell/record declarations replace false generic outputs; accepted nominal-input instantiation, rejected output refinement, typed stores and public tool paths have focused passing tests. Parsed CHECK! publication now preserves live type terms for lowering certificates.

Finish the remaining honest declaration migration exposed by warm replay and production library loads. Do not weaken parametric checking. Keep rejected-program coverage and run the final combined checker/native suites. The old load-rejection harness still needs diagnosis of E-JSON-SYNTAX after strict fixes; generated-declaration capacity preflight also has a remaining baseline failure.


Update 2026-09-12 06:05 EEST (rowan): five suites fail on 495dea80 with the strict rule in force and their declarations not yet updated: cast-suite (rc 1), pre-trust-defer (rc 1), snapshot-xt-cell-decl (rc 70), p2-map-rewind (rc 70), addrmap-inline (rc 70; the tier stack deletes this one with the inliner); test/gate-debug.f prop-test also (prop-pc declares a parametric length over raw storage; hazel folds the concrete effect). None reach test/run.f because the pool stops at its first red (habu-run-every-registered dot). Finishing this dot means those suites green with concrete effects or the rule refined, and docs/forth.md stating the rule (hazel has the doc line).

Suites red for this cause on the complete run of 2026-09-12 (engine 04701ef9, exit code, first refusal): compiler-ir-id, compiler-ir-id-manifest, compiler-ir-id-proof (70: hook: non-certified definition: family-id at 'TFAM:TFAM-PKG$'); tail-pure-fixtures (70: box-alloc at '+'); effect-read-api (70: twovar at 'swap'); effect-store-census (70: sht-slot at '+'); snapshot-xt-cell-decl (70: row-cell at '+'); dictionary-record-shapes (70: drs-cell@ at '@'); ddc-verify (70: path! at '!'); xref (70: xrt-expect-found at 'TTRUE'); decl-event (70: rt-fld-cell at 'DEVTX.FLDTOK'); native-gate-aot-negative (70: rec-wid@ at '@'); addrmap-inline (70: ami-one at 'AMI-DATA'); p2-map-rewind (70: p2m-z at 'P2M-SINK'); bootstrap-wide-memory-src (70: bwm-rd64 at 'or'); lit-emit-size (70: sizes at 'BODY'); require-cap (70: forge-c at '!'); pre-trust-defer (1: E-UNDEFINED: PTDX-POS); cast (1: F18); native-gate-debug (1: FAIL: prop-test); prop (1: primitive semantic case failed). Twenty-one of the root's 53 measured reds (LESSONS.md, habu-run-every-registered-56d4962d).

Claim: agent=hazel workspace=.jj-ws/habu-integrate-strict-parametric-c39bbc70 (2026-09-12).

Landing 2026-09-12 (hazel): 15 of the 21 green with concrete effects. Left red, by cause: cast-suite (F18/F19), addrmap-inline (F3-F8) and p2-map-rewind (F2/F11) assert inlined engine-word and cast calls, which the tier stack retires with the inliner (habu-land-the-tier-* landing); effect-store-census F25 asserts NODES = SHAPES, which cannot hold on a snapshot-booted engine whose node-intern table starts empty and is dropped on USIGS-GROW (habu-rebuild-the-node-intern dot); compiler-ir-id F64-F71 finds CHECKER-VERIFY-PKG-START/-DONE callable at top level although checker.f:816 says they stay internal, because their zero-cell PRIM rows give SIG-MIN-IN 0 and internal-mark.f skips them (habu-seal-the-verify dot); pre-trust-defer patches a copied source tree that the seeded product never reads (habu-run-pre-trust-defer dot).
