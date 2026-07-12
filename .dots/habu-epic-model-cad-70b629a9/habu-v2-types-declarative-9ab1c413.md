---
title: "V2 types: declarative family schema reflection"
status: open
priority: 2
issue-type: task
created-at: "2026-07-11T10:25:02.721752+02:00"
blocks:
  - habu-checker-capability-derive-23788e95
---

Problem: MODEL-CAD-V2-PLAN.md:438-458 needs checked metadata traversal so codecs, op schemas, docs, migrations, and diagnostics do not hand-maintain parallel field/variant knowledge. Bounded design/probe dot under 30 minutes. Fix: specify read-only typed reflection views and generated visit/fold operations over family metadata; hidden fields and mutable checker registries remain inaccessible. Acceptance: design covers sums/enums/products, rollback/snapshot identity, generic codec/diagnostic consumers, and misuse negatives; split implementation dots. Files: MODEL-CAD-V2-PLAN.md:438-458, src/core/type-family.f, src/core/sumtype.f, src/core/type-schema.f, docs/type-families.md. Verify: metadata round-trip and hidden-field rejection fixture plan.
