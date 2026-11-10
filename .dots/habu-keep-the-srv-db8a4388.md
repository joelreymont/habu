---
title: "Keep the server's FATAL text after PG retires a connection"
status: open
priority: 1
issue-type: task
created-at: "2026-11-01T22:02:33.719569+02:00"
---

Problem: when the server ends a session mid-statement (pg_terminate_backend, or the server shutting down), PG closes the connection and throws PG:E-EXEC with no text kept: src/lib/pg.f POLL-QUERY :961-966, QUERY-DONE :952, WAITING :884, TAKE-NOTIFIES :1063 and SEND-FAILED :1104 call ABANDON-SLOT (:853) and throw, and only CONNECT-REFUSED (:917) copies libpq's message first (KEEP-REFUSAL :909). The server's FATAL message (SQLSTATE 57P01, 'terminating connection due to administrator command') therefore reaches no caller: Maki's job runner logs only 'failed, code -9251; reconnecting' (maki src/job.f:486; src/db.f:57-63 reads PG:REFUSED$ for PG:E-QUERY alone). Acceptance: after a transport-loss E-EXEC the task's refusal text holds what libpq last reported for that connection, read through the same public word a refused statement's text is read through, and is empty when libpq reported nothing; pg.f's header says so; a pg-test case terminates a connection's backend from a second connection and reads the text after the throw (the cases at src/lib/pg-test.f:1338-1353 assert the code only). Files: src/lib/pg.f, src/lib/pg-test.f. Verify: pg-test through bin/hb --load test/run.f -- pg-test. Depends: none; shares pg.f with habu-route-libpq-notices-a88cc555, which lands first. Ownership: unassigned. Claim: unassigned.
