---
title: Refuse a row-polymorphic word that never runs its body
status: active
priority: 2
issue-type: task
created-at: "2026-10-28T20:57:25.923120+02:00"
---

Problem: tim's review of the PG rows merge (tim-pgrows rkpnulpt) mutated private RUN-ROWS (src/lib/pg-rows.f:339) to `ROWS-OR-THROW nip`, dropping the body quotation unrun; the checker still accepted it under ( R [ R PG:result -- S PG:result ] PG:result -- S PG:result ). A word that never runs its R-to-S body cannot produce a caller-chosen S, so R and S must stay distinct (rigid) inside the definition; accepting it suggests the checker unifies the declared row variables. Mutated tree: .jj-ws.noindex/tim-pgrows/.out/review/r2/tree/. Acceptance: a minimal reproducer (a word declared ( R [ R -- S ] -- S ) that drops the quotation) refused with a named code through the real load path; the declared row variables of a definition's own effect do not unify with each other; existing row-polymorphic library words still check. Files: the checker's row-variable handling (located by the reduction). Verify: the reproducer refused, pg-rows tests, full suite. Depends: none. Ownership: tim (rows/taint area), handed to carl to land. Claim: agent=tim.
