---
title: Delete legacy record surfaces
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:19:17.070197+02:00"
blocks:
  - habu-migration-tests-and-51d00332
  - habu-migration-libs-to-4e798110
  - habu-migration-tools-to-d4e8fcf8
  - habu-migration-maki-models-c965e65d
---

Hard-delete BEGIN-STRUCTURE, END-STRUCTURE, +FIELD, PTR-FIELD:, CFIELD:,
STRUCT-BYTE+, STRUCT-ACTIVE, PRODUCT, ;PRODUCT, VALUE-RECORD,
END-VALUE-RECORD, src/core/structures.f, and
src/core/structures-effects.f implementations, checker events, preverify
branches, load rows, effects, TRUST rows, generated fixtures, and aliases after
every consumer migration. CELL and PTR-VARIABLE must already live in independent
owners. Keep only precise error-only tombstones and allowlisted negative data.
