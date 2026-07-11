---
title: "V2 research: approximation proof domains"
status: open
priority: 3
issue-type: task
created-at: "2026-07-11T10:25:02.741798+02:00"
blocks:
  - habu-v2-types-rewrite-1e03cc0b
---

Problem: MODEL-CAD-V2-PLAN.md:537-548 proposes distinct exact, ULP, relative-error, and empirically licensed equivalence domains; current precision/tolerance flags cannot compose as rewrite evidence. Bounded design/probe dot under 30 minutes. Fix: specify domain families, composition rules, tolerance accumulation, device-golden licensing, and exact-domain exclusion for approximate rules. Acceptance: exact rewrite chain remains exact, approximate rule cannot satisfy exact evidence, composed tolerance is deterministic, and device evidence remains required. Files: MODEL-CAD-V2-PLAN.md:537-548, maki/precision.f, maki/golden.f, maki/report.f, maki/rewrite/. Verify: GELU/TF32/recompute proof fixtures designed.
