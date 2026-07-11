---
title: "V2 types: design nominal CAD index kinds"
status: open
priority: 1
issue-type: task
created-at: "2026-07-11T10:25:02.696747+02:00"
---

Problem: MODEL-CAD-V2-PLAN.md:189-229 identifies design/revision/node/plan/artifact/evidence/target handles and dim/shape/dtype/layout/space/stage/effect/region indexes that are all physically one cell; current raw n roles permit semantic swaps. This is a bounded design/probe dot under 30 minutes, not implementation. Fix: inventory current DEFTYPE/type-family kind machinery in src/core/checker.f, src/core/type-family.f, src/core/type-schema.f and maki public signatures; specify the smallest nominal-kind extension, bootstrap staging, diagnostics, and swapped-kind negative fixtures; split implementation slices. Acceptance: design records exact syntax/effects, registry/rollback/snapshot treatment, migration order, and concrete subdots. Files: MODEL-CAD-V2-PLAN.md:189-229, docs/type-families.md:281-347, src/core/type-family.f, src/core/checker.f, src/core/type-schema.f, maki/model-ir.f. Verify: minimal checker probes plus type-family registry tests.
