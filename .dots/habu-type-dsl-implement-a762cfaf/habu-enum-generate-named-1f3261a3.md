---
title: "ENUM: generate named constructors"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:14:05.104373+02:00"
blocks:
  - habu-enum-parse-full-39c0dc1b
---

Own ENUM constructor generation and focused ctor tests. Generate sealed FAMILY:VARIANT checked constructors from declaration-order named field schemas while preserving constructor package spelling, tag ordinal, generic substitution, and atomic publication. Add nullary, payload, nested, arity/type, and rollback regressions.
