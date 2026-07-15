---
title: Typed MIR node/input iterator over item-count
status: open
priority: 3
issue-type: task
created-at: "2026-07-15T13:10:49.644047+02:00"
---

Capability gap found by the vecnum2 lane 2026-07-15: maki code iterates MIR nodes/inputs via raw ?do bounds (MIR-N@ x4 + MIR-IN-COUNT@ in sched-key.f alone; identical loops in lower-mm/lower-launch/checkpoint/plan-ir/traffic/mem-plan), because the typed NODE-COUNT@/OPERAND-COUNT@ accessors (correctly) refuse laundering item-count back to n, CAD-NUM's projections are private, and no typed per-node input-count accessor exists (OPERAND-COUNT@ is the global total). Fix in maki/model-ir.f (the owner): a typed iterator surface - e.g. MIR:EACH-NODE ( R [ R MIR:node-id -- R ] -- R ) and MIR:EACH-INPUT ( R node [ R MIR:input-idx -- R ] -- R ) following VEC:EACH's quotation shape, plus a typed per-node input-count if consumers need it - so loop bounds never need raw counts; then retire the documented raw residuals in sched-key.f (SK-N's pinned raw return stays until its own consumers migrate). Acceptance: sched-key's 5 raw count reads replaced, pins byte-identical; at least one sibling file migrated as proof; count->n laundering still rejects. Files: maki/model-ir.f(+test), maki/sched-key.f(+test), one sibling. Verify: model-ir/sched-key suites, maki/test.f, full run.f. Ownership: model-ir surface. NOTE: several sibling files are sol lower-* territory - coordinate.
