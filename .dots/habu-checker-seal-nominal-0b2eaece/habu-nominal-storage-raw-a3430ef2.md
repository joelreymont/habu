---
title: "Nominal storage: raw variable kinds"
status: open
priority: 1
issue-type: task
created-at: "2026-07-12T16:08:25.881837+02:00"
---

Problem: here, create, variable and constant publish unrestricted polymorphic effects, so raw storage can mint any nominal family. Acceptance: add TVK-ANY and TVK-RAW to effect variables; mark raw definers and verifier-generated effects RAW; propagate through copy, freshening, unification, rollback, snapshot, native and bootstrap paths; RAW accepts plain scalar representation but rejects nominal atoms, arity-zero families, linear/layout values and pointers containing nominal state; numeric generic cells remain valid; no per-fetch lookup. Files: src/core/checker.f, src/habu/verify-source.f, src/habu/habu2.f, bootstrap checker/compiler mirrors, engine fixtures, docs/effects.md. Verify: red-first variable/create/constant/here laundering cases, numeric positives, native fixpoint, bootstrap and full test/run.f. Depends: none. Ownership: raw type-variable kind only; no typed definer or CAD migrations.
