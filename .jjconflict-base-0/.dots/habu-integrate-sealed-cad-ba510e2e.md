---
title: Integrate sealed CAD-NUM into V2
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:14:22.925742+02:00"
blocks:
  - habu-seal-cad-num-36dbeec6
  - habu-census-lower-shape-836287e4
---

Edge note 2026-07-17: blockers habu-migrate-core-maki-9e46089b,
habu-migrate-lowering-maki-56492851, habu-migrate-training-maki-2b170ed2
SATISFIED and removed - all three MIR count-caller waves landed
(0220114e/c1233223/d87c5f93).

Full context: after all CAD-NUM owners and caller waves land, integrate the authority once without mixing consumer edits. Fix only src/habu/habu2.f, MODEL-CAD-V2-PLAN.md, and tools/public-signatures-test.f: integrate the completed CAD-NUM owners, audit mints/projections and public packages, and remove obsolete legacy numeric/allocation entry exposure only after fresh censuses are empty. Acceptance: production V2 has no legacy global numeric cast or raw allocation boundary; the legacy-string census has added every resulting migration leaf to this dot before it closes; exact public signatures, bootstrap/fixpoint, Maki/PTX, and full native gates are green. Depends on core roles/arithmetic/seal, memory/string/vector/MIR owners, every caller wave, and both shape and legacy-string censuses.

GROOMED 2026-08-04 (dot-groom). Dangling blockers repointed. All three named migration dots -
habu-migrate-core-maki-9e46089b, habu-migrate-lowering-maki-56492851 and
habu-migrate-training-maki-2b170ed2 - are gone from the graph: commit c990d7aea "Close MIR
count-caller migration dots" closed and archived the three together. Those dependencies are
satisfied; nothing blocks this dot now.
