---
title: "Count a FILTER's null fraction once"
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T22:32:22.876408+01:00"
---

Problem: src/lib/sparql/emit.f writes a slot's IS NOT NULL beside a native equality a FILTER implies on the same column (the null test is written where no strict join = reads the column), so PostgreSQL counts that column's null fraction twice and underestimates the rows. Acceptance: a query whose FILTER implies a native = on a column that may hold NULL emits no IS NOT NULL on that column and answers as before; an ALONE test in src/lib/sparql-test.f shows it. Depends: none. Claim: unassigned.
