---
title: Migrate Model IR count roles
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-13T14:14:22.913086+02:00\""
blocks:
  - habu-seal-cad-num-36dbeec6
---

Full context: maki/model-ir.f exposes raw/legacy MAKI-prefixed node, slot, operand, and materialized counts. Fix: reopen MIR and add exactly NODE-COUNT@, SLOT-COUNT@, OPERAND-COUNT@, MATERIALIZED-COUNT returning CAD-NUM:item-count; preserve MIR:input-index, ref-pos, and operand-ref nominal identities; do not own MI storage migration. Acceptance: zero/max/rollback counts, index-versus-count checker negatives, correct-signature controls, old accessors retained until caller waves empty. Files: maki/model-ir.f, maki/model-ir-test.f. Depends on sealed CAD-NUM.

Claim: agent=mirnum workspace=.jj-ws/fable-cadnum
