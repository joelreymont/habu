---
title: "V2 types: artifact typestate and evidence design"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-11T10:25:02.710163+02:00"
closed-at: "2026-07-13T16:18:14.030152+02:00"
close-reason: "Design landed on master: R7 addendum MODEL-CAD-V2-PLAN.md:1280-1712 (commit d31f680c rebased from acbee023); adversarial review MERGE (probes independently reproduced: GATE! slot mixing report.f:592, forged PROMOTE-OK? cad.f:1026, store bypass store.f:355); acceptance fixtures + 6 implementation sub-dots minted (stage-a0eb43a2 -> evidence-f124dc85 -> promotion-d539e648 -> promotion-2266b236 -> report-df8e34fa, store-57afdc0a) with review advisories folded in"
---

Problem: MODEL-CAD-V2-PLAN.md:335-368 requires pass ordering and promotion evidence to be structural; current runtime report tags can be mixed across artifacts or bypassed by another call path. This is a bounded design/probe dot under 30 minutes. Fix: specify stage families for model/tensor-ir/region-ir/plan/kernel/candidate/artifact, artifact-indexed evidence families, promotion-policy products, and exact transition words; reuse existing SUMTYPE/PRODUCT/MATCH rather than inventing a parallel system. Acceptance: design contains untypeable wrong-order, incomplete-plan, wrong-artifact-evidence, and missing-gate fixtures and implementation subdots. Files: MODEL-CAD-V2-PLAN.md:335-368, maki/report.f, maki/cad.f:754-825, maki/store.f, docs/type-families.md. Verify: CHECK! candidate fixtures and current PROMOTE behavior census.

Claim: agent=arttype workspace=.jj-ws/fable-arttype
