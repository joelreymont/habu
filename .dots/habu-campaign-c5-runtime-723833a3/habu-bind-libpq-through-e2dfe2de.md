---
title: Bind libpq through the FFI with a typed DB layer
status: closed
priority: 1
issue-type: task
created-at: "2026-09-16T16:10:29.926595+03:00"
closed-at: "2026-09-17T03:44:25.329586+03:00"
close-reason: "implemented, reviewed, landed on hazel's line and integrated at d7555528, gate R green (417 of 417)"
---

Problem: the Tender server needs Postgres now and Habu has no database access; the decision in docs/database-models.md is libpq over the FFI with a hand-written typed layer above a generated binding. Acceptance: package DB in lib/db/pq.f over libpq (dlopen of libpq.so.5) declared through the FFI FUNCTION: declarer: nominal connection and result handles, a linear statement or result owner that must be cleared exactly once, CONNECT, EXEC, PREPARE, EXEC-PREPARED with typed parameters, row and column readers returning byte spans and typed numbers, WITH-TRANSACTION, CLOSE, and result ADTs carrying the server's SQLSTATE; no raw n for an error; one connection per task, statements never cross tasks. Tests against a local Postgres started by a fixture script under test/ (skipped with a named reason when no server is reachable) covering connect, DDL, parameterized insert and select, transaction rollback, and every failure arm. Files: lib/db/pq.f, lib/db/pq-test.f, docs/db.md, lib/errors.f codes in a new DB block. Verify: the tests on the integrator's engine. Depends: the FFI declarer (habu-pkg-owned-prim-e08e345f follow-up), habu-add-a-blocking-fd79b713. Ownership: lib/db/. Claim: unassigned.
