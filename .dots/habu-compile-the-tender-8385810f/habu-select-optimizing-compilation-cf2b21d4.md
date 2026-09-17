---
title: Select AOT before every executable dependency
status: closed
priority: 1
issue-type: task
created-at: "\\\"\\\\\\\"2026-09-11T16:07:57.859028+03:00\\\\\\\"\\\""
closed-at: "2026-09-16T14:34:48.800623+03:00"
close-reason: "superseded by habu-campaign-c1-finish-1f129a00: Executable builds still leave helper JIT spans because AOT mode is not held across require/include/evaluate and generated words"
blocks:
  - habu-build-engine-layout-abdd0188
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: Cedar, `.jj-ws/cedar-closure-identity`.

Own app-image.f first-action selection, hb-build-lib.f/build-fixpoint.f generated build entries and image fixture routing; consume native-build API without competing driver edits. Current APP-IMAGE tail selection leaves helper JIT spans. Hold executable-build mode through require/include/evaluate/immediates/generated words. Preserve ordinary --load/REPL JIT. Verify documented direct APP-IMAGE, stripped/REPL hb-build, native-build and child captures with JIT compiler entry forbidden before dependencies. Earlier retained JIT must refuse; fixture-only tier prelude cannot hide public API failure. No interpreter or replay workaround.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Review 2026-09-13, snapshot51546316: Current review D02 independently confirms the direct APP-IMAGE first-require defect at frozen51546316. Use a cold process with dependencies absent and a generated definition/immediate tier-0 attempt; a warm preload is not evidence. Native-build now owns its driver scope, but direct APP-IMAGE still needs this repair.

Direct APP-IMAGE now selects tier1 first, then loads its implementation and all
dependencies through EXECUTABLE-BUILD:WITH. The hb-build REPL stream invokes a
small checked driver from outer stdin after its include returns; that driver
holds the same scope through application load, generated startup and SAVE.
On tracked B2 cd88273b, the actual cold load's complete new code interval has
native origin1, and an application parsing immediate that requests tier0 is
refused by the build guard before an output exists. Existing snapshot capture
then reaches the separate 32768 address-row ceiling (1ca5db10). Full capture,
stripped build and all entry-path acceptance remain open.
