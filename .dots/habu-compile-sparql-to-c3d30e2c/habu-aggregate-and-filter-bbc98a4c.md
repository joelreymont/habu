---
title: Aggregate and filter SPARQL natively at scale
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T18:43:18.857485+01:00"
---

Problem: every SPARQL aggregate but COUNT builds its group's terms as jsonb in one array_agg for sparql.aggregate (emit.f AGGREGATE$, COMBINED$), and every FILTER operator is a generic call on jsonb terms (emit-expression.f), even over a native typed column; Kai asks for sums over 5.74 million rows of one kind and date ranges over its largest kinds. Acceptance: a SUM, AVG, MIN and MAX over 5,000,000 rows of a typed column and a range filter keeping 1% answer within twice the native SQL's time and memory, answers unchanged. Depends: none.
