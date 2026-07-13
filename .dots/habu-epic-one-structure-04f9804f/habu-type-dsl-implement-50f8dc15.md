---
title: "Type DSL: implement typed STRUCTURE"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:45:34.385823+02:00"
blocks:
  - habu-type-dsl-unify-b65d46c1
---

Extend STRUCTURE ... ;STRUCTURE into the sole record definer. Parse arity, typed named fields, nested family types, pointer/byte fields, POLICY, and DERIVE clauses transactionally. Generate checked PACKAGE:MAKE and PACKAGE:UNMAKE plus typed field accessors from the shared schema. Lower each STRUCTURE to the internal product kind, including generic instantiation, width, offsets, alignment, and hidden-field expansion. Replace the old raw offset-only behavior rather than layering a second record DSL. Add declaration, generics, nested-field, layout, and negative regressions.
