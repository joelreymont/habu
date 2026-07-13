---
title: "Fields: add shared schema arena"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:12:36.155644+02:00"
blocks:
  - habu-type-dsl-specify-db2bf883
---

Own src/core/type-field.f, its focused suite, load list, and FILEMAP row. Add one transactional field record keyed by family, optional variant, and field name with schema root, cell/byte layout, alignment, and flags. Add typed reflection queries plus duplicate/reserved-name negatives. Validate focused load, filemap, host, and typed diff lints.
