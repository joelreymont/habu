---
title: "Type DSL: unify field metadata"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:45:25.258711+02:00"
blocks:
  - habu-type-dsl-specify-db2bf883
  - habu-fields-prove-transactional-7a74c018
---

Replace separate raw-structure, value-record, product-field, and variant positional-payload metadata with one transactional named field-schema representation keyed by owning declaration plus optional variant. Preserve nominal family identity, generic parameter schemas, cell offsets, byte layout, alignment, visibility, rollback watermarks, snapshot identity, and read-only typed reflection. Reuse the internal type-family product/sum algebra; do not expose those kinds as public definers. Add metadata round-trip, duplicate/reserved field, rollback, and snapshot regressions.
