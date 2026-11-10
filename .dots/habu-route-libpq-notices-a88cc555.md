---
title: Route libpq notices through LOG, not bare stderr
status: active
priority: 1
issue-type: task
created-at: "2026-11-01T20:50:32.850038+02:00"
---

Problem: src/lib/pg.f installs no libpq notice receiver or processor (no PQsetNoticeReceiver/PQsetNoticeProcessor anywhere in it), so libpq's default writes server notices and asynchronous errors to fd 2 as bare text, outside LOG's contract (src/lib/log.f:1-8: a failure caused outside the program is logged once, as one tagged line). Found from Maki's r2-runner lane: after pg_terminate_backend of an idle connection the image's stderr carries the bare line 'FATAL:  terminating connection due to administrator command' (libpq hands an error that arrives outside a query to the notice processor), beside Maki's own LOG lines such as '2026-11-01T18:45:48Z error project-store: FATAL:  too many connections for role ...' (maki .jj-ws.noindex/r2-runner/.out/mut-restartflagged.log:23-24). Repro: open two PG connections, pg_terminate_backend the first one's pid from the second, then PG:LIVE? or a query on the first: stderr gets the bare FATAL line. Acceptance: no libpq text reaches fd 2 except as a well-formed LOG line; an asynchronous error on a connection is logged at most once; what PG does with notices below error level (NOTICE, WARNING) is stated in pg.f's header. Regression: a pg-test case that terminates a connection's backend in a child process and asserts the child's stderr holds no bare libpq line (it fails on master). Files: src/lib/pg.f, src/lib/pg-test.f. Verify: check.f on pg.f; pg-test. Depends: none. Ownership: src/lib/pg.f notice handling, its test.

Consumers: met by Maki today (a bare FATAL line beside its tagged log lines). Claim: agent=bob workspace=.jj-ws.noindex/bob-pgnotice.
