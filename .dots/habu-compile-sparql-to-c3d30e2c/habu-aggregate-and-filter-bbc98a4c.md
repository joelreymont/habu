---
title: Filter SPARQL natively at scale
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T18:43:18.857485+01:00"
---

Problem: a comparison of dates or instants is still a generic call on jsonb terms (src/lib/sparql/emit-expression.f: sparql.equal, sparql.relation, sparql.in): the store has no date or instant class a native comparison can take, Zen's date and instant shapes declare no `type`, and a literal without a zone has no native value; Kai asks for date ranges over its largest kinds. Acceptance: a date range filter keeping 1% of 5,000,000 rows of a typed column answers within twice the native SQL's time and memory, answers unchanged, and a test per lowered operator on dates and instants compares its answers with the generic call's. Depends: none.
