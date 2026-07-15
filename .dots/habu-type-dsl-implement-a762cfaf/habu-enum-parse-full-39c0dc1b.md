---
title: "ENUM: parse full and compact forms"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:13:59.138732+02:00"
blocks:
  - habu-type-dsl-unify-b65d46c1
  - habu-type-dsl-implement-50f8dc15
---

Own src/core/enum-decl.f and declaration tests. Consume shared syntax events for
numeric-arity full mode with optional headers and VARIANT/FIELD/;VARIANT, plus
compact ENUM name variant ... ;ENUM selected by the first bare variant with
implicit arity zero and no headers. Reject mixed modes, an arity followed by
compact variants, compact headers, positional payloads, missing delimiters,
duplicates, reserved names, and malformed arity transactionally.
Own the exact native and recovery post-hook load rows for src/core/enum-decl.f
in src/habu/habu2.f and bootstrap/cg/forth.fs; do not modify unrelated loader
rows.
