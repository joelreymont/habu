---
title: "Keep a row-polymorphic provider's declared row across a tier-1 compile"
status: active
priority: 2
issue-type: task
created-at: "\"2026-09-13T11:45:43.535614+03:00\""
---

Problem: compiling a row-polymorphic higher-order provider at tier 1 leaves its recorded declared row bound to the EMPTY row, so a caller with a non-empty prefix is refused while an empty-prefix caller passes; the provider's tier decides (provider at tier 1 fails at either caller tier, provider at tier 0 is clean at both); the trigger is a body applying a quotation with an explicit row through finally, while the same body with execute is clean (execute is the one kind QUOT-WINDOW freezes early); the tier-1-only state is REC-ON, armed by CHECKER-TAPE:ARM, which enables U-CALL-TAIL / CALL-FREEZE inside unification. Reduced 2026-09-13 by the tier-stack lane to a 40-line reproduction with no rebuild (scratchpad files qB.f and ry2.f), reproducing on the root engine with '1 set-tier'; it stops the product rebuild at src/compiler/ir/context.f WITH-CONTEXT-BOUND, the first compiler file the tier-1 window compiles, and so blocks the tier stack's selfbuild, its product rebuild and the chain. Acceptance: the recorded row of a tier-1-compiled provider is the declared row variable, not its empty instance (the freeze or tail unification that binds it is fixed at the responsible layer in src/core/checker.f); a regression through the real load path compiles the provider at tier 1 and calls it from both an empty-prefix and a non-empty-prefix caller at both tiers; the compiler suites and test/run.f unchanged on a seed cold build; byte identity and the chain. Files: src/core/checker.f (REC-ON, U-CALL-TAIL, CALL-FREEZE, QUOT-WINDOW), test/. Verify: the regression, the suites, the chain, test/run.f. Depends: none. Ownership: hazel. Claim: unassigned

Parked 2026-09-13 (hazel, account limit): unstarted beyond reduction; reproductions in .jj-ws/habu-keep-a-row-f2c4f3d4/build/repro/ (qB.f, ry2.f and the v-*/m-a-* variants the worker made), reproduce on root engine 28e11361 with 1 set-tier. Blocks the tier stack's product rebuild.

Cedar review 2026-09-13 on root engine 28e11361: the finally-only diagnosis is too narrow. Existing build/repro/p-only.f and v-quot-exec.f also fail at tier1 with an execute wrapper; v-direct.f and e-only.f pass, p-only-t0.f passes. Cover quotation-wrapped providers as well as finally; do not repair only the finally primitive. No implementation change in this review.
