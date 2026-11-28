---
title: Write only the columns a SPARQL query reads
status: open
priority: 2
issue-type: task
created-at: "2026-11-26T23:27:29.333731+01:00"
---

Problem: a CTE PostgreSQL materializes (one read twice, a BY-BRANCH CTE, a walk's recursive CTE) carries every bound variable's columns, read or not, so PostgreSQL computes and stores values no reader reads: a walk's rest CTE carries `v0 = t1.s` unread. Acceptance: such a CTE writes no column its readers do not read; every SPARQL test answers as before; EXPLAIN (VERBOSE) of a walk query shows no CTE Scan or WorkTable Scan output that no parent reads. Depends: none.
