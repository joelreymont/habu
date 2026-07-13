---
title: "Checker: certify unified type DSL"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:45:50.509869+02:00"
blocks:
  - habu-type-dsl-implement-50f8dc15
  - habu-type-dsl-implement-a762cfaf
---

Teach the checker, preverify, all-errors replay, candidate scopes, and generated-constructor protocol only STRUCTURE/FIELD/;STRUCTURE and ENUM/VARIANT/FIELD/;VARIANT/;ENUM. Type MAKE/UNMAKE, field access, constructors, and MATCH from shared schema metadata. Preserve fail-closed declaration transactions and emit field/variant-specific structured diagnostics. Add minimal positive definitions and negative wrong-field, wrong-variant, arity, non-exhaustive, malformed-declaration, rollback, and same-width-role regressions.
