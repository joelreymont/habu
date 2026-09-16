---
title: Elide identity moves of the data-stack pointer
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:38:23.446409+03:00"
---

Problem: tier 1 emits `sub x19,x19,#16` immediately followed by `add x19,x19,#16` with nothing between (measured 2026-09-16 in SOURCE-QPATH-CHECK, docs/compiler-measurements.md), the data-stack twin of the frame identity copy that MB-IDENTITY-COPY? already removes for spill slots; the census shows sp loads and stores barely moved between tiers (6,483 to 6,324) because stack-pointer adjustments are planned per block edge and not cancelled across them. Acceptance: the planner cancels an adjustment of x19 that is undone before any use of the stack memory it exposed, and merges adjacent adjustments into one; a fixture pins the shape (two consecutive words each consuming and producing the same depth); the census pair shows the sp-adjust count drop and no benchmark regression; byte fixpoint; full gate green. Files: src/compiler/native/regalloc.f, spill.f, select.f, test/compiler/*.f. Verify: tools/tier-census.f before and after; tools/tier-bench.f; tools/native-build.f fixpoint; test/run.f. Depends: habu-inline-trivial-engine-922133ca lands first (same emitter region). Ownership: tier-1 register planner. Claim: unassigned. Source: measurement lane, ranked fix 3.
