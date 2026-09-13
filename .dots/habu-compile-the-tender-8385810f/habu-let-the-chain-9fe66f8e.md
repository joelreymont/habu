---
title: "Build generations privately without moving bin/hb"
status: open
priority: 2
issue-type: task
created-at: "2026-09-13T09:43:28.051549+03:00"
blocks:
  - habu-build-engine-layout-abdd0188
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own tools/two-generation-build.f output routing, consuming native-build output API, and Generation Chain docs. Copy entry engine to HB_TMP-derived private directory; build all products there. Never move/remove installed bin/hb. Verify success, invalid source/output and interrupted scratch-tree run leave entry path/SHA unchanged, then complete chain. Existing tool lost bin/hb twice. No second chain implementation.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Review 2026-09-13, snapshot51546316: Current review D07 also covers the native-build default: RUN still selects bin/hb without an output argument. The private-output contract must cover that entry directly, before two-generation routing. A no-output invocation must leave the accepted image unchanged; requiring one explicit candidate path is sufficient. Do not add a promotion framework.
