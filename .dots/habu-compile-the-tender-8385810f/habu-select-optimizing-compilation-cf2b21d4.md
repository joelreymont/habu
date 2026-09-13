---
title: "Select AOT before every executable dependency"
status: open
priority: 1
issue-type: task
created-at: "\"2026-09-11T16:07:57.859028+03:00\""
blocks:
  - habu-build-engine-layout-abdd0188
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own app-image.f first-action selection, hb-build-lib.f/build-fixpoint.f generated build entries and image fixture routing; consume native-build API without competing driver edits. Current APP-IMAGE tail selection leaves helper JIT spans. Hold executable-build mode through require/include/evaluate/immediates/generated words. Preserve ordinary --load/REPL JIT. Verify documented direct APP-IMAGE, stripped/REPL hb-build, native-build and child captures with JIT compiler entry forbidden before dependencies. Earlier retained JIT must refuse; fixture-only tier prelude cannot hide public API failure. No interpreter or replay workaround.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Review 2026-09-13, snapshot51546316: Current review D02 independently confirms the direct APP-IMAGE first-require defect at frozen51546316. Use a cold process with dependencies absent and a generated definition/immediate tier-0 attempt; a warm preload is not evidence. Native-build now owns its driver scope, but direct APP-IMAGE still needs this repair.
