---
title: "Nominal storage: migrate CAD owners"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T16:08:58.389635+02:00"
blocks:
  - habu-nominal-storage-typed-c5f44d66
---

Problem: fusion, Model IR, tensor and lowering staging owners still use generic raw storage plus private nominal refinements. Acceptance: migrate FP-RID and nominal staging cells, Model IR descriptor columns, tensor-value descriptor columns and actual target/toolchain nominal storage to TYPED-VARIABLE or TYPED-BUFFER; counters/raw numeric columns stay generic; remove obsolete raw projections and test corruption seams; discharge corresponding TRUSTED rows; retire LAYOUT-BUFFER only after every consumer migrates. Files: maki/fusion-plan.f, model-ir.f, tensor-value.f, lower-launch.f and focused callers/tests, TRUSTED.md, docs. Verify: same-family round trips, target/toolchain, node/region, dtype/layout/address-space swap rejection, typed-local/trust lints, maki/full gates. Depends: habu-nominal-storage-typed-c5f44d66.
