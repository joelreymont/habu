---
title: Prune SPARQL sources a constant cannot fit
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T10:51:27.752693+01:00"
---

Problem: FITS? (src/lib/sparql/emit.f) passes every column slot, so a constant predicate such as rdfs:label also compiles branches over Zen's 7 knowledge.sparql_direct sources per graph, whose name shape's in() can never hold it; PostgreSQL proves them empty at plan time, at 21 ms of planning against 0.5 ms without them (/tmp/carl-sparql/hunt-union-report.md). The store description cannot say which terms a shape holds. Acceptance: those branches are not emitted; sparql-test.f covers a pruned and a kept branch; Zen's description says it. Depends: the drive-unions dot. Claim: unassigned.
