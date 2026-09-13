---
title: "Plan combine once and fix measured spill scaling"
status: open
priority: 2
issue-type: task
created-at: "\"2026-09-11T16:38:06.270563+03:00\""
blocks:
  - habu-walk-the-dynamic-e03edf85
---

Plan: [PLAN.md](../../PLAN.md). Design reconciled 2026-09-13; replaces stale diagnosis/claim. Claim: unassigned.

Own native/combine.f/spill.f and focused tests/chain-scale cases. Rebase172fc17d on current lifecycle; duplicate REWRITES/FUSIONS versus REWRITE planning is defect, module already rebuilds at most once. Seal plan for exact input, consume once, invalidate release/refusal, preserve fold precedence/use/order/immediate checks. Keep four spill maps/descending work already landed. Count F-NEED visits on all-AOT baseline; if material use once-enqueued predecessor reachability for Boolean fixed point. Preserve allocation/rewrite fixed point and validator. Test stale/no-plan, no-change identity, real/call/quotation/control/KEEP spills and negatives. Require established slopes<=1.1; load12 reading is invalid.

Verification: focused real-load cases above; rebuild and run `bin/hb --load test/run.f` for compiler/runtime integration. Speed acceptance uses the all-AOT campaign pair; functional/count evidence can be developed in parallel.

Review 2026-09-13, snapshot51546316: Current review F02 confirms duplicate analysis in REWRITES/FUSIONS and REWRITE; the module is already rewritten at most once. Retain the no-op module and one-use plan for the exact input. F04: lowering/allocation is a required fixed point; no arbitrary one-pass/two-round truncation.
