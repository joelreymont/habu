---
title: Integrate sealed CAD-NUM into V2
status: open
priority: 2
issue-type: task
created-at: "2026-07-13T14:14:22.925742+02:00"
blocks:
  - habu-seal-cad-num-36dbeec6
  - habu-migrate-maki-allocation-3335b802
  - habu-migrate-test-allocation-0e295089
  - habu-migrate-tool-allocation-22c04eb4
  - habu-migrate-str-numeric-2febad4b
  - habu-migrate-vector-numeric-360069d6
  - habu-migrate-model-ir-c171bdf5
  - habu-migrate-core-maki-9e46089b
  - habu-migrate-lowering-maki-56492851
  - habu-migrate-training-maki-2b170ed2
  - habu-census-lower-shape-836287e4
  - habu-census-legacy-str-b84390fe
---

Full context: after all CAD-NUM owners and caller waves land, integrate the authority once without mixing consumer edits. Fix only src/habu/habu2.f, FILEMAP.md, TRUSTED.md, MODEL-CAD-V2-PLAN.md, STATUS.md, and tools/public-signatures-test.f: load sealed lib/cad-num.f, register owner/test files, audit mints/projections and public packages, remove obsolete legacy numeric/allocation entry exposure only after fresh censuses are empty. Acceptance: production V2 has no legacy global numeric cast or raw allocation boundary; the legacy-string census has added every resulting migration leaf to this dot before it closes; exact public signatures/trust/filemap/status, bootstrap/fixpoint, Maki/PTX, and full native gates green. Depends on core roles/arithmetic/seal, memory/string/vector/MIR owners, every caller wave, and both shape and legacy-string censuses.
