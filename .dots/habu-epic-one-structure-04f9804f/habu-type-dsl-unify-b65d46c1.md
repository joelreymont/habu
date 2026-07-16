---
title: "Type DSL: unify field metadata"
status: active
priority: 1
issue-type: task
created-at: "\"2026-07-13T16:45:25.258711+02:00\""
blocks:
  - habu-migration-core-records-77182600
  - habu-fields-prove-transactional-7a74c018
  - habu-type-declarations-shared-14ab0e48
---

Replace value-record, product-field, and positional variant-payload metadata with
one transactional named field-schema representation keyed by owning declaration
plus optional variant. Own the shared STRUCTURE/ENUM syntax-event transaction;
the two post-hook front ends consume it without duplicating parser state or
publication logic. Preserve nominal family identity, generic parameter schemas,
cell offsets, byte layout, alignment, visibility, rollback watermarks, snapshot
identity, and read-only reflection. Reuse internal product/sum algebra without
public definers. Add metadata, parser-event, rollback, and snapshot regressions.

Claim: agent=change-file-controller workspace=.jj-ws/habu-change-file-integration.
