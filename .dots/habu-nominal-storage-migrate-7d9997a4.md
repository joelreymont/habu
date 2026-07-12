---
title: "Nominal storage: migrate CAD owners"
status: open
priority: 2
issue-type: task
blocks:
  - habu-nominal-storage-typed-d261e51e
created-at: "2026-07-12T15:48:44.825673+02:00"
---

Phase 4 of habu-checker-seal-nominal-0b2eaece after typed definers. Migrate fusion FP-RID and lowering staging cells, Model IR nominal descriptor columns, tensor-value nominal descriptor columns, and actual target/toolchain owner storage to TYPED-VARIABLE or TYPED-BUFFER. Keep counters and raw numeric columns generic. Remove obsolete raw projections and test corruption seams, discharge corresponding TRUSTED rows, and retire LAYOUT-BUFFER only after all consumers migrate. Acceptance: same-family storage works; target/toolchain, node/region, dtype/layout/address-space swaps reject; owner/maki/full gates green.
