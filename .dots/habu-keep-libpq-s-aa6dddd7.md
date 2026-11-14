---
title: "Keep libpq's text when ENDED? finds a connection gone"
status: open
priority: 2
issue-type: task
created-at: "2026-11-10T23:09:35.996008+02:00"
---

Problem: src/lib/pg.f ENDED? closes a connection for a failure libpq reported (a failed PQconsumeInput or CONNECTION_BAD) and answers true, dropping libpq's message; the E-EXEC sites keep theirs for PG:REFUSED$ (habu-keep-the-srv-db8a4388). Acceptance: after ENDED? answers true for a connection whose backend was ended, PG:REFUSED$ holds what libpq reported for it, or is empty when libpq reported nothing; ENDED?'s comment says so; a case under the PG harness ends a backend from a second connection and reads the text. First observe over 20 runs what libpq reports on this path (an error that arrives outside a query may go to the notice processor, habu-route-libpq-notices-a88cc555, and leave only the end-of-stream text). Files: src/lib/pg.f, src/lib/pg-rows-test.f. Verify: bin/hb --load test/db/pg-cluster.f. Depends: habu-keep-the-srv-db8a4388 landed. Ownership: unassigned. Claim: unassigned. Consumers: Maki calls PG:ENDED? (src/service.f:161) and logs no reason; not reported by Maki as a failure.
