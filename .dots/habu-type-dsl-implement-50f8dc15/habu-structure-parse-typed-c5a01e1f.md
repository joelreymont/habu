---
title: "STRUCTURE: parse typed declarations"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:13:20.867544+02:00"
blocks:
  - habu-type-dsl-unify-b65d46c1
---

Own src/core/structure-decl.f and declaration tests. Implement mandatory arity, optional POLICY/DERIVE, repeated FIELD name type, and ;STRUCTURE as one provisional transaction over shared family/schema/field metadata. Reject malformed, duplicate, reserved, unresolved, and mixed legacy tokens. Load after checker/type-family; do not preserve or wrap raw BEGIN-STRUCTURE.
