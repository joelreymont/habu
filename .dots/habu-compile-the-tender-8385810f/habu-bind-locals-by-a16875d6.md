---
title: Bind locals by one case rule in checker and compiler
status: closed
priority: 1
issue-type: task
created-at: "\"2026-09-12T00:57:25.366743+03:00\""
closed-at: "2026-09-13T16:25:50.462035+03:00"
close-reason: "Integrated55455720 with separate Astra review; both JIT/AOT local-case and native-elaborate pass on fresh0c602d1c. Full315-suite run passed local-case and exposed two callers relying on old case mismatch: ZIP path fixture fixedc25bd570, existing ZIP tests pass; ACAP-DEFER-SITE fixed81a7ceef after separate review, real capture suite and product-hosted build from825c2c60 pass, product968cabff also passes capture. Remaining full-gate failures stay explicit under campaign and named leaves; no all-AOT/speed or final green claim."
---

Plan: [PLAN.md](../../PLAN.md). Owner: Cedar local-case implementation lane, .jj-ws/cedar-local-case. Existing September12 finding promoted to P1 after the user independently reproduced it on both Maki and current integration, September13.

Problem: the checker resolves a reference to a live local case-insensitively while the legacy (tier 0) compiler resolves it case-sensitively, so a mixed-case reference certifies as the local and runs the word. Reproducer on the root bin/hb (found by Maki's shadow lint, 2026-09-12): with global ': G-STEP ( -- n ) 4 ;' and package public ': P-WIDTH ( -- n ) 3 ;', ': SAME ( n -- n ) {: P-WIDTH:n :} P-WIDTH ;' answers 9 for 9 SAME, but ': MIXED-PKG ( n -- n ) {: p-width:n :} P-WIDTH ;' answers 3 and ': MIXED-GLOBAL ( n -- n ) {: g-step:n :} G-STEP ;' answers 4, both certified as ( n -- n ) with no warning; docs/forth.md promises certification and execution name the same word. In Maki three guards (VIEW-READ, OUTLINE, PLACE-ALL) compared a local to the word it shadowed and the checker certified a comparison that was always false. Acceptance: one rule at both layers, case-insensitive like every other Habu lookup: the tier 0 compiler's local binding folds the reference the way the checker does, the tier 1 pipeline is proved to agree (a test compiling the three definitions on each tier, both answering 9 for MIXED-PKG and MIXED-GLOBAL), and a reference that resolves to a local at one layer and a word at the other is impossible by construction rather than linted; docs/forth.md states the rule (a local binds every spelling of its name for its scope) and that a local inside a quotation is refused (E-BAD-LOCAL-SHAPE). Files: src/habu/habu2.f (local binding), src/core/checker.f (local resolution), src/compiler/native/elaborate.f (proof only), test/, docs/forth.md. Verify: the tier test; test/run.f. Dependencies: none for local-lookup implementation; work is confined to local comparison sections, separate from tier dispatch. Reconcile source before final integration. Claim: active.

Current acceptance includes REVIEW-WIDTH lowercase/mixed/uppercase references returning the bound17 instead of visible word3, matching checker/JIT/AOT, duplicate locals differing only by case, package/global shadowing and quotation-scope rejection. Own local lookup sections in engine/bootstrap mirror, native elaboration and raw-local comments/tests. Do not change provider call recording or use a Maki lint as repair.

Integrated55455720 after independent Astra review. Fresh private engine0c602d1c194c
from the combined local-case/arena source passes `native-local-case.f` and
`native-elaborate.f`; the dedicated regression checks both compiler tiers.
The full native suite ran315 suites in `.jj-ws/cedar-correctness-verify`.

The repaired lookup exposed one capture caller collision: `ACAP-DEFER-SITE`
declared `cell` and used `cell CELL +`, intending the global cell-size constant.
Both spellings now correctly bind the local, causing the false `defer metadata
outside DATA window` refusal. Rename the address local to `addr`; retain `CELL`
and the bounds check. No second mixed-case local collision was found in this file.

Attribution: unchanged source `ccc0661a`, built twice from host `28e11361`,
produced `45b5b4eb` then `2fc1a47e`, both rc0. This refusal is an exposed caller
bug, not evidence for the separate layout task. On host `825c2c60`, the existing
`aot-chain-capture` suite failed before the rename and passed afterward. The
private native build of `181a01cf` plus this rename also passed from that host,
producing `968cabff`, whose real capture suite passes too. These are functional
results, not a byte-fixpoint claim.
