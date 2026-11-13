---
title: Tier 1 reloads loop-invariant locals in every iteration
status: open
priority: 2
issue-type: task
created-at: "2026-11-13T10:15:56.241136+02:00"
---

Problem: ARRAY:A-SUM's tier-1 loop (tier-bench arith) reloads loop-invariant locals from the frame on every iteration, about 5% of the loop's time on M2 Max. Acceptance: a loop-invariant local stays in a register across the loop; arith's median drops and no tier-bench benchmark slows. Depends: none. Claim: unassigned.
