---
title: "Type DSL: delete legacy definers"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:46:53.544223+02:00"
blocks:
  - habu-migration-tests-and-51d00332
  - habu-migration-maki-models-c965e65d
  - habu-delete-legacy-type-36040d18
---

Hard-delete TYPEFAMILY, PRODUCT, ;PRODUCT, SUMTYPE, ;SUMTYPE, VALUE-RECORD,
END-VALUE-RECORD, BEGIN-STRUCTURE, END-STRUCTURE, +FIELD, PTR-FIELD:, CFIELD:,
STRUCT-BYTE+, STRUCT-ACTIVE, ENUM+, ENUM4+, positional VARIANT payload parsing,
src/core/structures.f, src/core/structures-effects.f, src/core/enums.f, their
checker/native/recovery branches, generated-source emitters, effects, TRUST
rows, docs, and aliases. PTR-VARIABLE and CELL must already have independent
owners and do not keep legacy files alive. Keep internal algebraic kinds only.
Acceptance: case-insensitive token-aware scans find spellings only in the
error-only tombstone table, explicitly allowlisted non-executable negative
fixtures, and migration history; runtime lookup finds no removed definer.
