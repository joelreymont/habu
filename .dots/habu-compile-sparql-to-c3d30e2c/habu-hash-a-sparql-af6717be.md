---
title: Hash a SPARQL join on a sure variable
status: closed
priority: 2
issue-type: task
created-at: "2026-11-27T16:51:25.287279+01:00"
closed-at: "2026-11-27T17:55:29.230678+01:00"
---

Problem: a join on a variable bound by BIND(COALESCE(?o, "")) or a GROUP BY (e AS ?v) key is written (a IS NULL OR a = b) (src/lib/sparql/emit.f +NULL-OR) because SURE? knows only aggregates and GROUP-KEY marks such a key maybe, so PostgreSQL runs it as a nested loop; Tender's survey joins grouped subqueries on keys made that way. Acceptance: such a join's statement names no IS NULL OR, EXPLAIN shows a Hash Join on it, and every answer is unchanged. Depends: none.
