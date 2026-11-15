---
title: Drop a PG connection libpq lost at query end
status: open
priority: 3
issue-type: task
created-at: "2026-11-04T11:56:43.519596+02:00"
---

Finding (PG send-fail lane worker, 2026-11-04; read, not reproduced): src/lib/pg.f QUERY-DONE (:1069 at the lane tip) fills the last result and returns PG-PROGRESS:completed without asking libpq whether the connection survived. When libpq loses sync with the server (the reply it reads is malformed or cut), it marks the connection bad and returns an error result; QUERY-DONE reports that as a completed query and keeps the dead connection in its slot, so the next query on it fails at send (now closed by SEND-FAILED) instead of here. Fix at QUERY-DONE: after the last result, a connection libpq reports bad (PQstatus) abandons the slot as SEND-FAILED does. Test first: a fake TCP4 peer in src/lib/pg-test.f that answers a query with a cut or malformed message (the REFUSED-SEND-CASES shape) and expects the query to fail and the slot freed. Verify: test/db/pg-cluster.f, test/db/pg-kill-test.f. Depends: none. Claim: unassigned.
