---
title: Return from a PG COPY statement instead of draining forever
status: active
priority: 2
issue-type: task
created-at: "2026-11-04T11:07:43.747946+02:00"
---

Problem: `c s" copy (select 1) to stdout" [: ;] WITH-RESULT` never returns: /tmp/carl-pgsend/probe4.f under test/db/pg-cluster.f prints 'before copy', then the case deadline expires (rc 67, throw -2502). In COPY_OUT/COPY_IN state libpq's PQgetResult answers a fresh PGRES_COPY_* result on every call and never NULL, so POLL-QUERY's drain loop (src/lib/pg.f:942-951) keeps replacing DRAIN-LAST and never reaches QUERY-DONE. Acceptance: a COPY statement sent through PG either is refused with a stated error before it is sent or ends the query with a result the caller can read (decide which by what WITH-RESULT promises at pg.f:1483); the connection stays usable for the next statement; a row in the PG cluster test (test/db/pg-cluster.f) runs the probe's statement and a following `select 1`. Also fix the stale comment above BACKEND-COUNT in src/lib/pg-test.f ('The server-side boundary: closing a pending connection…' describes CANCEL-CASES). Files: src/lib/pg.f, src/lib/pg-test.f. Verify: TMPDIR=<scratch> bin/hb --load test/db/pg-cluster.f </dev/null rc 0 with the new row, and the row fails at master. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-pg.
