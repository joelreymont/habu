---
title: "V2 R3: type stage effect region kinds"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T07:09:24.403035+02:00"
blocks:
  - habu-v2-r3-declare-3fcdeebb
  - habu-checker-seal-nominal-0b2eaece
  - habu-checker-seal-owner-f7de26ff
---

Problem: pipeline stage, op effect, and fused-region indexes are raw n across maki lowering/planning, so wrong-stage artifacts and region/effect swaps can pass the stack checker. Fix: migrate the smallest owning registries and public signatures to CAD-KIND:stage, CAD-KIND:effect, and CAD-KIND:region, with private validated projections at array/dispatch boundaries. Acceptance: stage/effect, stage/region, and effect/region swaps reject; one legal region lowering chain certifies; diagnostics name the qualified kinds; no public owner API returns raw n for these roles. Files: maki/op-registry.f, maki/fusion-plan.f, maki/lower-*.f focused owner files and tests. Verify: focused registry/fusion/lowering tests, maki/test.f, typed-local diff lint. Depends: CAD kind declarations.
