---
title: "V2 research: schedule legality evidence"
status: open
priority: 3
issue-type: task
created-at: "2026-07-11T10:25:02.737667+02:00"
blocks:
  - habu-v2-types-artifact-6ee556f8
---

Problem: MODEL-CAD-V2-PLAN.md:518-536 proposes solver-produced vectorizable/fits-smem/occupancy/mma evidence so lowering can require exact legality rather than rechecking raw fields. Bounded design/probe dot under 30 minutes. Fix: define evidence families indexed by plan/target and identify which Plan IR verifier facts can become constructors without making estimates into proofs. Acceptance: design separates exact legality from estimated profitability, prevents target/plan evidence reuse, and lists lowering signature migrations. Files: MODEL-CAD-V2-PLAN.md:518-536, maki/schedule.f, maki/mem-plan.f, maki/lower-mm.f, lib/ptx/. Verify: wrong-target/wrong-plan negative fixture design.
