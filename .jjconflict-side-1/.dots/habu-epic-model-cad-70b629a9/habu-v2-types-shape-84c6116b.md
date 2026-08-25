---
title: "V2 types: shape-polymorphic quotation design"
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T10:25:02.725859+02:00"
blocks:
  - habu-checker-shape-kind-4c6a3f4c
---

Problem: MODEL-CAD-V2-PLAN.md:476-488 needs generic checked passes/visitors/schedule predicates over indexed tensors and existential shapes; current quotations cannot declare/open these relations. Bounded design/probe dot under 30 minutes. Fix: specify declared indexed quotation effects, lexical existential opening, local capture restrictions, and effect/capability visibility by extending existing checked higher-order machinery. Acceptance: generic map/visitor positive, escaped existential negative, wrong shape relation negative, and loop/call-chain preservation fixtures; split implementation dots. Files: MODEL-CAD-V2-PLAN.md:476-488, src/core/checker.f, src/core/combinators.f, docs/forth.md quotation rules, .dots/habu-multishot-quotations-typed-8832cace.md. Verify: CHECK! quotation probes.
