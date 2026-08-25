---
title: V2 compile latency ratchets
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T12:14:25.717772+02:00"
blocks:
  - habu-single-pass-checking-aabfb874
---

Problem: MODEL-CAD-V2-PLAN.md:1514-1528 requires sub-second bounded edit-to-report and explicit hot-path budgets; current gates do not attribute dependency-cone, parse/check/lower, or cache-hit latency. Fix: add phase timers and ratchets for clean compile, one-node edit, no-op/cache-hit, and report generation, then remove duplicated work exposed by the first profile. Acceptance: one-node edit touches only its dependency cone; cache-hit produces identical artifact hash; budget regression fails with an exact phase. Files: maki/pass.f, maki/store.f, tools/bench*.f, test/gate*.f. Verify: cold/hot benchmark fixture and full gate budget row.
