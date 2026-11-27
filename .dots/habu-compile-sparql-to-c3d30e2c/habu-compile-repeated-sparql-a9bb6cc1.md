---
title: Compile repeated SPARQL aggregates within 1 MiB
status: closed
priority: 2
issue-type: task
created-at: "2026-11-27T22:59:57.804123+01:00"
closed-at: "2026-11-28T00:05:59.787990+01:00"
---

Problem: Joel: every compile of a query of at most 16,384 characters maps at most 1 MiB. On test/sparql/wide-store.json, SELECT (COUNT(?o) AS ?c) WHERE { ?s zen:has ?o } GROUP BY ?s ORDER BY COUNT(?o) ... with 1,500 ORDER BY keys (15,088 characters) is accepted and maps 2,097,152 bytes, its statement 561,696 bytes; over the ?s ?p ?o union, 1,500 keys map 2,097,152 too. Each key adds a sort key and a grouped column; the cause is not yet found. Acceptance: such queries map at most 1 MiB at every size up to 16,384 characters, an emit-test.f family shows it, every SPARQL test answers as before. Depends: none.
