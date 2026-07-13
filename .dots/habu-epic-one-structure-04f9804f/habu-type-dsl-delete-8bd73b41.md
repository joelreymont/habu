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

Hard-delete PRODUCT, ;PRODUCT, SUMTYPE, ;SUMTYPE, VALUE-RECORD, END-VALUE-RECORD, BEGIN-STRUCTURE, END-STRUCTURE, ENUM+, ENUM4+, positional VARIANT payload parsing, their checker events, native/Gforth/bootstrap branches, generated-source emitters, TRUST rows, documentation, and compatibility aliases. Reserve removed tokens so use fails with a precise migration diagnostic rather than resolving as user words. Keep internal algebraic product/sum kinds only. Acceptance: authoritative token-aware scan finds removed tokens only in the dedicated rejection fixtures and migration history; runtime lookup cannot find any removed definer.
