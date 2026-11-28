---
title: "Join a keyed SPARQL union by PostgreSQL's choice per arm"
status: open
priority: 2
issue-type: task
created-at: "2026-11-28T13:41:59.124267+01:00"
---

Problem: a union joined to rows that bind its subject (`?m rdfs:label ?l . ?m ?p ?o`) is a LATERAL probed once per key and branch (emit.f UNION-STEP, DRIVER): on the sized fixture the count took 5 s, with 20 table kinds (337 branches) 45 s, though PostgreSQL hashes the same join in 1-6 s when each branch is its own join. Acceptance: the bound rows are planned once and each source's branches are one join PostgreSQL plans itself; that count answers in under 6 s with 20 table kinds and under 1.5 s on the sized fixture at the load measured beside it, answers unchanged, no TEST-HEAVY family's largest accepted size smaller. Depends: none.
