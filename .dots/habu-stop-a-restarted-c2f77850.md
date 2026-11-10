---
title: Stop a restarted postgres when pg-cluster kills a case
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T18:11:05.741662+02:00"
---

Problem (tim from Maki, 2026-11-06; found by reading, not reproduced by a killed run): test/db/pg-cluster.f spawns postgres itself so the server stays in the pool's process tree (START, :221-226: pg_ctl's postmaster calls setsid and is reparented to init). A case that restarts the server through pg_ctl (Maki's test/project-restart-case.f: pg_ctl restart, stop, start -o '-k SOCKETS -c listen_addresses=') leaves a postmaster that is pg_ctl's, not SERVER. When pg-cluster ends the case at its bound (CASE-KILL, :303-308, PROC-TREE:KILL-TREE of the case engine), the case's own cleanup never runs, STOP signals SERVER's old pid (gone), and the restarted postmaster keeps running on the removed data directory. Details: /Users/joel/Work/maki/.jj-ws.noindex/tim/.out/carl-pg-restart.md. Acceptance: a case killed at its bound after restarting the server leaves no postgres running: either a case restarts, stops and starts the server through pg-cluster so it stays pg-cluster's child, or pg-cluster stops whatever postmaster owns its data directory (postmaster.pid) on every exit; a test kills a case after a pg_ctl restart and finds no process on the data directory. Files: test/db/pg-cluster.f and its test. Verify: that test; Maki's restart case still passes. Depends: none. Ownership: test/db/pg-cluster.f. Claim: unassigned.

Consumers: Maki no longer runs cases under pg-cluster and has no restart case, so no consumer meets this today.
