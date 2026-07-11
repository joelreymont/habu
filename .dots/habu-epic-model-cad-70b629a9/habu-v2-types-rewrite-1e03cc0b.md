---
title: "V2 types: rewrite equivalence evidence design"
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T10:25:02.717583+02:00"
blocks:
  - habu-v2-types-artifact-6ee556f8
---

Problem: MODEL-CAD-V2-PLAN.md:419-437 marks independent rewrite evidence beneficial; optimizer-selected flags are not replayable proof that input and output graphs agree. Bounded design/probe dot under 30 minutes. Fix: define exact equivalence family indexed by input/output/domain, audited axiom constructors, symmetry/transitivity/congruence, proof DAG storage, and independent replay; separate approximate domains. Acceptance: design proves wrong dtype/layout/domain cannot construct evidence, axiom version enters cache keys, and search cannot self-certify; split implementation dots. Files: MODEL-CAD-V2-PLAN.md:419-437, maki/fusion-plan.f, maki/adjoint.f, src/core/type-family.f. Verify: candidate proof-composition fixtures and replay test design.
