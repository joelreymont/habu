---
title: Rename the libpq binding package DB to PG
status: open
priority: 2
issue-type: task
created-at: "2026-09-22T16:52:42.243756+03:00"
---

Owner: Cedar, assigned by Joel. Tender owns package DB; Habu's libpq boundary
becomes package PG in lib/pg.f, with its live suite in lib/pg-test.f. Rename
the error namespace, library registry entries, documentation and consumers.
Acceptance: the PG and combined FFI suites pass, Tender compiles against PG,
and the new source/engine pair is announced with the landed commit.
