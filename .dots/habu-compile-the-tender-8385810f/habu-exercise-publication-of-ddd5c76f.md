---
title: Execute published namespaced generated constructors
status: closed
priority: 2
issue-type: task
created-at: "\"2026-09-13T14:51:13.085488+03:00\""
closed-at: "2026-09-16T14:34:50.149201+03:00"
close-reason: "superseded by habu-campaign-c3-the-a2477c89: Residue: no fixture publishes a checked generated constructor in a package, executes it, and rejects a same-tail foreign type"
blocks:
  - habu-keep-a-row-f2c4f3d4
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own test/type-ctor-suite.f namespace fixture; touch generated constructor publication owner only if this real behavior fails. Current zpl test pins inconsistent body/effect without publication/execution. Publish checked constructor in package, execute/observe payload, add other-package same-tail type/constructor and wrong-type rejection. Coverage gap, not established runtime defect. Verify type-ctor/namespace/effect suites; no new language construct or weakened effect.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.
