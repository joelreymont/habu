---
title: "Fields: attach variant ranges"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:12:42.577284+02:00"
blocks:
  - habu-fields-add-shared-6b063c62
---

Own SUMV metadata in src/core/type-family.f and focused family tests. Replace positional payload schema ranges with shared named-field ranges while preserving tag ordinal, payload width, constructor symbol/package, rollback, and family identity. Do not change public syntax. Validate type-family and rollback suites.
