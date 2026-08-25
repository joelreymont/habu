---
title: "V2 research: proof-producing bounded e-graph"
status: open
priority: 3
issue-type: task
created-at: "2026-07-11T10:25:02.733472+02:00"
blocks:
  - habu-v2-types-rewrite-1e03cc0b
---

Problem: MODEL-CAD-V2-PLAN.md:507-517 marks proof-producing equality saturation as optional research after deterministic V2 rewrites. Bounded design/probe dot under 30 minutes. Fix: specify regional node/enode/fuel caps, deterministic union/extraction, proof-edge retention, effect/type guards, and cache identity; whole-model unbounded saturation is forbidden. Acceptance: design includes termination, deterministic extraction, proof replay, cap diagnostics, and comparison against deterministic passes; split implementation only after V2 rewrite evidence exists. Files: MODEL-CAD-V2-PLAN.md:507-517, maki/fusion-plan.f, future maki/rewrite/. Verify: small algebraic region model and cap probes.
