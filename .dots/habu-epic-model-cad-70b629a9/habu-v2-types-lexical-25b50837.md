---
title: "V2 types: lexical region ownership design"
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T10:25:02.705907+02:00"
blocks:
  - habu-tfam-11-linear-99fa9990
---

Problem: MODEL-CAD-V2-PLAN.md:304-334 needs shared immutable graph references plus unique transaction/device ownership; current linear tokens prove consume-once but do not express owner-scoped immutable/mutable references. This is a bounded design/probe dot under 30 minutes. Fix: define checker-generated lexical region tokens, arena-owner, arena-ref, arena-mut, borrow overlap/escape rules, and interaction with current linear/path-sensitive tracking; split implementation slices. Acceptance: design includes owner-drop-with-live-ref, double-mut, mutation-through-ref, transaction-commit, and device lifetime fixtures without proposing general lifetime inference. Files: MODEL-CAD-V2-PLAN.md:304-334, src/core/checker.f, docs/effects.md, docs/type-families.md:1362-1392, lib/memory.f, maki/cuda-driver.f. Verify: focused symbolic checker probes and linear suite plan.
