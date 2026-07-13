---
title: Delete legacy record surfaces
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:19:17.070197+02:00"
blocks:
  - habu-migration-tests-and-51d00332
  - habu-migration-core-records-77182600
  - habu-migration-libs-to-4e798110
  - habu-migration-tools-to-d4e8fcf8
  - habu-migration-maki-models-c965e65d
---

Hard-delete BEGIN-STRUCTURE, END-STRUCTURE, +FIELD, PTR-FIELD:, PRODUCT, ;PRODUCT, VALUE-RECORD, and END-VALUE-RECORD implementations, checker events, preverify branches, load rows, effects, and aliases after every consumer migration. Keep only precise reserved-token rejection.
