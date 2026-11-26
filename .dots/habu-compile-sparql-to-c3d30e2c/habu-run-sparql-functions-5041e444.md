---
title: Run SPARQL functions in parallel workers
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T18:43:18.847300+01:00"
---

Problem: sparql.floating, float_arithmetic, cast, matches, replaced and aggregate (src/lib/sparql/runtime.sql) are PARALLEL SAFE yet hold an EXCEPTION block, whose subtransaction PostgreSQL rejects in a parallel worker, so a query whose plan runs one there fails. Acceptance: each runs under debug_parallel_query with answers unchanged. Depends: none.
