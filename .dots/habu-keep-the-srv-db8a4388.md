---
title: "Keep the server's FATAL text after PG retires a connection"
status: open
priority: 3
issue-type: task
created-at: "2026-11-01T22:02:33.719569+02:00"
---

When a backend is terminated mid-statement (pg_terminate_backend, or the server ending the session), PG retires the connection and throws PG:E-EXEC (src/lib/pg.f ~929-935, ~997-1003) with REFUSED$ already emptied, so the server's FATAL message (e.g. 57P01 terminating connection due to administrator command) reaches no caller and no log; Maki's job runner can log only 'failed, code -9251; reconnecting'. Keep the server's last error text (SQLSTATE and primary message) readable after a transport-loss throw, as REFUSED$ is after E-QUERY. Related: habu-route-libpq-notices-a88cc555 (libpq writes the same FATAL to fd 2). Reported by tim (Maki), 2026-11-01; nothing in Maki waits on it. Owner: carl.
