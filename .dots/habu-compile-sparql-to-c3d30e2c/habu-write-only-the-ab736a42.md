---
title: Write only the columns a SPARQL query reads
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T23:27:29.333731+01:00"
---

Problem: every derived table, UNION ALL branch and CTE body of a compiled statement carries every bound variable's columns, read or not, so PostgreSQL materializes wider rows (design /tmp/carl-sparql/design-tree.md, READS). Acceptance: a column no reachable expression reads is not written, except under DISTINCT, UNION, GROUP BY or a read with *; every SPARQL test answers as before; EXPLAIN of the seq-16 shape shows narrower CTE rows. Depends: habu-write-sparql-s-9021d6f9.
