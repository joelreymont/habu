---
title: "Type DSL: implement typed STRUCTURE"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:45:34.385823+02:00"
blocks:
  - habu-type-dsl-unify-b65d46c1
  - habu-structure-prove-generic-efb21e9c
---

Implement the sole post-hook STRUCTURE ... ;STRUCTURE record definer. Consume
the shared declaration-event transaction to parse mandatory arity, typed named
fields, nested family types, pointer/byte fields, POLICY, and DERIVE clauses.
Generate checked PACKAGE:MAKE and PACKAGE:UNMAKE plus typed field accessors from
the shared schema. Lower each declaration to the internal product kind. Add no
cold parser, transient descriptors, adoption path, bootstrap-only semantics, or
raw offset definer. Add declaration, generics, nested-field, layout, and
negative regressions.
