---
title: Clear a met SPARQL column in every binding
status: closed
priority: 2
issue-type: task
created-at: "2026-11-28T02:21:08.161049+01:00"
closed-at: "2026-11-28T02:34:09.148462+01:00"
---

Problem: MEET's strict branch (src/lib/sparql/emit.f, MEET) clears only the scope binding's nullable flag, so another binding on the same column keeps its test beside the strict `=`: on store-a, GRAPH zen:history { ?s zen:id ?o . ?t zen:id ?o } writes t1.id = t2.id AND t1.id IS NOT NULL AND t2.id IS NOT NULL. Acceptance: that query writes neither test; every SPARQL test answers as before; no measured shape's statement grows. Depends: none.
