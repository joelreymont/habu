---
Problem: `lib/pg.f` uses blocking `PQconnectdb` and `PQexec*`, so a Tender
worker occupies its pthread while PostgreSQL connects or answers. Tender already
has Habu's AIO io_uring loop for HTTP and transfers; the database path should
yield on the same readiness mechanism.

Acceptance: package `PG` keeps the existing checked connection/result handles
and public `CONNECT`, `EXEC`, `SCRIPT`, `PREPARE`, `EXEC-PREPARED`, transaction,
parameter and read vocabulary, but uses libpq's nonblocking calls and
`AIO:POLL-ADD`/`AIO:AWAIT` for connection and query progress. Every socket
wait handles readable and writable progress, flushes pending output, drains
all results, preserves diagnostics and result ownership, and closes/cancels
cleanly. The PostgreSQL wire protocol remains libpq's responsibility.

Verification: focused Habu PG tests against a throwaway PostgreSQL instance
cover connect, refusal, parameterized queries, prepared queries, multi-result
drain, transaction rollback and socket waits; Tender's DB suite runs with a
slow-query fixture proving another HTTP worker remains responsive. Run the
Habu package gate and Tender's server build against the same integration tip.
title: Drive libpq through AIO
status: open
priority: 2
issue-type: task
created-at: "2026-09-22T17:21:10.018550+03:00"
---
