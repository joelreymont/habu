---
title: Filter SPARQL natively at scale
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T18:43:18.857485+01:00"
---

Problem: every FILTER comparison (=, !=, <, <=, >, >=, IN) is a generic call on jsonb terms (src/lib/sparql/emit-expression.f EQUAL+, ORDERED+, IN+: sparql.equal, sparql.relation, sparql.in), even where both operands are a typed shape's native column or a literal of its type; Kai asks for date ranges over its largest kinds. Acceptance: a range filter keeping 1% of 5,000,000 rows of a typed column answers within twice the native SQL's time and memory, answers unchanged, and a test per lowered operator compares its answers with the generic call's. Depends: none.
