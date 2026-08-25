---
title: "Fields: retire product rows"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:12:48.390051+02:00"
blocks:
  - habu-fields-attach-variant-151e2713
---

Own PF metadata in src/core/type-family.f and focused family tests. Move product field names, schemas, slots, offsets, and widths onto the shared field arena; remove the separate PF store and its rollback/snapshot watermarks. Preserve existing internal product semantics. Validate type-family, rollback, and snapshot suites.
