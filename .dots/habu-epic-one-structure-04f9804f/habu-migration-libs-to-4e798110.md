---
title: "Migration: libraries to unified types"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:46:21.449737+02:00"
blocks:
  - habu-migration-core-records-77182600
  - habu-migration-core-variants-af8e09b4
  - habu-libs-migrate-option-510a7e40
  - habu-libs-migrate-process-6bfe40be
  - habu-libs-migrate-map-e52318f9
  - habu-libs-migrate-runtime-c2738a4d
  - habu-libs-migrate-ptx-1071a2e6
---

Migrate lib declarations and APIs to STRUCTURE and payload-capable ENUM with named fields. Delete VALUE-RECORD, PRODUCT, SUMTYPE, positional variant payload, and legacy structure syntax from lib. Preserve public package APIs, nominal identities, binary layouts, and tests. Replace numeric closed domains encountered during migration with ENUM while retaining external wire codes behind exhaustive codecs. Run every owning lib suite and its real-consumer gates.
