---
title: "Type DSL: implement payload ENUM"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T16:45:43.405762+02:00"
blocks:
  - habu-type-dsl-unify-b65d46c1
  - habu-enum-expose-named-5bfe8bb0
---

Extend ENUM ... ;ENUM into the sole variant definer. Support named inline FIELD declarations inside VARIANT blocks, generic parameters, nested record/enum fields, POLICY, and DERIVE clauses. Reuse the exact STRUCTURE field schema; never synthesize hidden record families for variants. Infer tag-only layout when every variant is payloadless and tagged-union layout otherwise. Generate checked constructors and exhaustive MATCH effects with field-aware diagnostics/reflection. Reject mixed positional payloads, duplicate fields, and malformed shorthand.
