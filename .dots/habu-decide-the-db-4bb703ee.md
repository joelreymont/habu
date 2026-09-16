---
title: Decide the database package model
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T13:16:06.964897+03:00"
---

Problem: Habu has files, JSON, XML, zip and hash maps but no records, fields, indexes or SQL, and both the Love Me Tender server and embedded targets need persistent structured data. polyFORTH's Data Base Support kit (contiguous files, fixed-length records, named typed fields, ordered indexes with BINARY/-BINARY/+ORDERED/-ORDERED, chains with FIRST/-NEXT/CHAIN/UNCHAIN, one current file and one current record) is the Forth-native model; VFX ships SQLite only as a mechanical extern binding of sqlite3.h with no Forth vocabulary (docs/database-models.md). Acceptance: a decision recorded in docs/database-models.md choosing a typed record kit in the polyFORTH shape over a Habu file or flash region, an FFI SQLite binding with Postgres later for hosted programs, or both behind one field vocabulary; it names the public words and stack effects, the storage format, the concurrency rule (one copy of a record, a facility around index maintenance) and the first consumer, and it opens the implementation dots. Files: docs/database-models.md. Verify: the doc answers every point above and the child dots exist. Depends: none. Ownership: docs/database-models.md. Claim: unassigned.
