---
title: "ENUM: parse full and compact forms"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:13:59.138732+02:00"
blocks:
  - habu-type-dsl-unify-b65d46c1
---

Own src/core/enum-decl.f and declaration tests. Implement numeric-arity full mode with VARIANT/FIELD/;VARIANT plus payloadless compact mode selected by a nonnumeric first variant. Reject mixed modes, positional payloads, missing delimiters, duplicates, reserved names, and malformed arity transactionally.
