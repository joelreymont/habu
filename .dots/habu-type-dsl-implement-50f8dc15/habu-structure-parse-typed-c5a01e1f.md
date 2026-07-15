---
title: "STRUCTURE: parse typed declarations"
status: open
priority: 1
issue-type: task
created-at: "2026-07-13T17:13:20.867544+02:00"
blocks:
  - habu-type-dsl-unify-b65d46c1
---

Own src/core/structure-decl.f and declaration tests. Consume the shared syntax
events for mandatory arity, optional POLICY/DERIVE, repeated FIELD name type,
and ;STRUCTURE as one provisional transaction over shared metadata. Reject
malformed, duplicate, reserved, unresolved, and mixed legacy tokens. Load only
after render and check-hook. No cold parser, descriptor, adoption path, or raw
BEGIN-STRUCTURE wrapper is permitted. Own the exact native and recovery
post-hook load rows for src/core/structure-decl.f in src/habu/habu2.f and
bootstrap/cg/forth.fs; do not modify unrelated loader rows.
