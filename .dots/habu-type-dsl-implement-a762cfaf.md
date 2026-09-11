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

Implement the sole post-hook ENUM ... ;ENUM variant definer. Full mode requires
numeric arity and VARIANT blocks and may use POLICY/DERIVE; compact mode is
exactly ENUM name variant ... ;ENUM with implicit arity zero and no header
clauses. Consume the shared declaration-event transaction and STRUCTURE field
schema; never synthesize hidden record families. Infer tag-only layout only
when every variant is payloadless. Generate checked constructors and exhaustive
MATCH effects. Reject mixed modes, positional payloads, duplicate fields,
compact headers, and malformed full declarations transactionally.
