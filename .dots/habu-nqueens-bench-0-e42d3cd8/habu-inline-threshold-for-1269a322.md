---
title: Inline threshold for known-loop helper
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-02-24T17:56:13.917574+01:00\\\"\""
closed-at: "2026-02-24T18:32:43.694783+01:00"
close-reason: "Rejected after A/B: no win and regression risk; reverted backend trial"
---

src/jit/backend.zig:3628-3644 currently inlines only countIrNodes<=30 for non-recursive known callees. RCA: NQUEENS-SAFE-P is nodes=31 and stays as cross call from NQUEENS-SOLVE, paying call/prologue overhead in hot loop. Implement a bounded generic threshold policy (constant + guardrails), verify SAFE-P inlines (trace/dump), add regression, and rebaseline nqueens10 vs SBCL.
