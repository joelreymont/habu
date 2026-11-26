---
title: Bind a float8 parameter in PG
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T14:57:41.502110+01:00"
---

Problem: src/lib/pg.f binds parameters with TEXT+, INT+, NULL+ and BINARY+ only, so Maki carries its own float8 binder (maki src/db.f F64-PUT and F64+, :737-743; tim's request 2026-11-26, /tmp/tim-pg-float8.md); text is no substitute, since PostgreSQL's float8 input rejects 1e-400. Acceptance: PG:F64+ ( connection r -- ) binds the value as a binary float8 parameter (OID 701), so $n::float8 and an untyped $n in a float8 context read it exactly, NaN and the infinities included, with a pg-test case; Maki drops its copy. Depends: none. Claim: unassigned.
