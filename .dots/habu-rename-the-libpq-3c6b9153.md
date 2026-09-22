---
title: Rename the libpq binding package DB to PG
status: open
priority: 2
issue-type: task
created-at: "2026-09-22T16:52:42.243756+03:00"
---

Asked by cedar (Tender dot 6f89f29a): Tender wants to own package DB and needs the Habu libpq boundary under a name that says what it is. lib/db/pq.f is `package DB` (:35) - rename it to `package PG`; the file stays lib/db/pq.f. Also lib/db/pq-test.f (package DB-TEST and its DB: references), the lib/errors.f comment on the DB error range (:1406), docs that name package DB, and every in-repo consumer (rg -n "DB:" lib test tools docs). Consumers outside the repo: Tender (cedar migrates DB: to PG: after the announcement). Acceptance: lib/db/pq-test.f and the gate green; commit body lists the renamed public surface; announced with the integration it lands in. Ownership: hazel.
