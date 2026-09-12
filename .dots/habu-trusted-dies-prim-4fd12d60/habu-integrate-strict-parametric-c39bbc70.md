---
title: Integrate strict parametric effects and checked Forth helpers
status: open
priority: 1
issue-type: task
created-at: "2026-09-10T18:03:13.337312+03:00"
---

Owner: Cedar; nominal_pointer handles the remaining reduced compiler binding failure. Strict nominal/generic pointers and type-variable kinds are integrated. Concrete byte/cell/record declarations replace false generic outputs; accepted nominal-input instantiation, rejected output refinement, typed stores and public tool paths have focused passing tests. Parsed CHECK! publication now preserves live type terms for lowering certificates.

Finish the remaining honest declaration migration exposed by warm replay and production library loads. Do not weaken parametric checking. Keep rejected-program coverage and run the final combined checker/native suites. The old load-rejection harness still needs diagnosis of E-JSON-SYNTAX after strict fixes; generated-declaration capacity preflight also has a remaining baseline failure.


Update 2026-09-12 06:05 EEST (rowan): five suites fail on 495dea80 with the strict rule in force and their declarations not yet updated: cast-suite (rc 1), pre-trust-defer (rc 1), snapshot-xt-cell-decl (rc 70), p2-map-rewind (rc 70), addrmap-inline (rc 70; the tier stack deletes this one with the inliner); test/gate-debug.f prop-test also (prop-pc declares a parametric length over raw storage; hazel folds the concrete effect). None reach test/run.f because the pool stops at its first red (habu-run-every-registered dot). Finishing this dot means those suites green with concrete effects or the rule refined, and docs/forth.md stating the rule (hazel has the doc line).
