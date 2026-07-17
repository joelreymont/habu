---
title: "Fields: add shared schema arena"
status: active
priority: 1
issue-type: task
created-at: "2026-07-13T17:12:36.155644+02:00"
blocks:
  - habu-migration-core-records-77182600
---

Own src/core/type-field.f, its focused suite, load list, and FILEMAP row. Add one transactional field record keyed by family, optional variant, and field name with schema root, cell/byte layout, alignment, and flags. Add typed reflection queries plus duplicate/reserved-name negatives. Validate focused load, filemap, host, and typed diff lints. Claim: agent=habu-fields-schema-v2 workspace=.jj-ws/habu-fields-schema-v2.
