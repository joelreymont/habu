---
title: Let the application declare the DB connection registry capacity
status: open
priority: 2
issue-type: task
created-at: "2026-09-22T18:47:16.555989+03:00"
---

Problem (aspen, Tender job-pools lane blocked): lib/db/pq.f:77 '$08 constant CONN-CAP' caps the connection registry at 8 per process and CLAIM-CONN-SLOT (pq.f:444) throws DB:E-CAPACITY past it. tenderd holds 3 connections (main, writer, sched) and every pool worker task opens its own because package DB binds a connection to the task, so a deployment runs 5 job workers where the design needs 14 (2 listers, 8 fetchers, 2 unpackers, 2 extractors) and a test suite adds 2 more. Reproducer with the frozen pair in its header: ~/.cache/tender/habu-gaps/db-conn-cap/conn-cap-repro.f (run.log beside it: asks 16, gets 8, 'assert: expected 16 got 8'; run with test/server/migrations.sh python3 scripts/habu.py run conn-cap-repro.f from a Tender workspace). Acceptance: the registry capacity is declared by the application or grown on demand from the arena, not a larger constant (Tender needs 20 today and the number is the deployment's); the reproducer passes at 16; lib/db tests green; aspen told the commit so Tender moves its pin. Files: lib/db/pq.f, its tests. Verify: the reproducer; lib/db suites; test/run.f. Ownership: cedar (libpq is cedar's per Joel). Claim: unassigned.
