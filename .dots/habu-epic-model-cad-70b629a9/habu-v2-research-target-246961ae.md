---
title: "V2 research: target and dialect generics"
status: open
priority: 3
issue-type: task
created-at: "2026-07-11T10:25:02.746074+02:00"
blocks:
  - habu-v2-types-design-70831db1
---

Problem: MODEL-CAD-V2-PLAN.md:549-559 proposes bounded generics so one checked lowering template can quantify over target capability/dialect families when a second backend exists. Bounded design/probe dot under 30 minutes. Fix: inventory duplicated target/dialect lowering structure and specify capability-bounded generics without unrestricted higher-kinded types. Acceptance: design names the second concrete consumer, required kind/capability terms, monomorphization/lowering path, diagnostics, and rejection when no second consumer exists. Files: MODEL-CAD-V2-PLAN.md:549-559, src/arch/ptx/, maki/lower-*.f, future maki/target/. Verify: two-target MRE required before implementation dots.
