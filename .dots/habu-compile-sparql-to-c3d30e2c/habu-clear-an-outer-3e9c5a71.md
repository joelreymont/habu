---
title: "Clear an outer meeting's column in every binding"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-28T03:21:08.718359+01:00"
closed-at: "2026-11-28T03:36:08.804970+01:00"
---

Problem: src/lib/sparql/emit.f OCCUR's strict meeting with a binding the substitution always binds drops only that occurrence's NULL; another binding of the triple's occurrences on the same column keeps its IS NOT NULL in the same WHERE, against the header's per-column rule. On store-a, SELECT ?f WHERE { ?f zen:evidence ?p FILTER EXISTS { GRAPH zen:history { ?f zen:id ?o } } } writes t2.id = t1.id AND t2.id IS NOT NULL inside the EXISTS. Acceptance: that query and its slots swapped (?o zen:id ?f) write no null test of the met column inside the EXISTS, and a test of sparql-test.f fails without the fix. Depends: none.
