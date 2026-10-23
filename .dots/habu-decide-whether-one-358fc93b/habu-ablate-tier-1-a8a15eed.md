---
title: Ablate tier-1 passes on real workloads
status: open
priority: 2
issue-type: task
created-at: "2026-10-23T18:58:18.974188+03:00"
---

Build engine variants (never landed) each with one tier-1 pass off - the loop fold, whole-word data-stack residency (select.f, flushing at joins as a single pass would), copy coalescing - and time the workloads of 'Time real workloads at both tiers' on each. The run-time cost of each pass on real code is what a single pass would lose. Depends: the census and timing children. Claim: unassigned.
