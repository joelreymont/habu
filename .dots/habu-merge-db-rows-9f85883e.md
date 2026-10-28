---
title: Merge DB-ROWS into PG
status: open
priority: 2
issue-type: task
created-at: "2026-10-28T20:27:29.571875+02:00"
---

Joel 2026-10-28 (interview): DB-ROWS is PG-only, so src/lib/db/rows.f becomes src/lib/pg-rows.f in package PG, every DB-ROWS:X becomes PG:X (hard cutover, rows-test.f moves too). New public: PG:EXEC ( PG:connection ptr u8 n -- ) a statement answering no rows; PG:QUERY ( R PG:connection ptr u8 n [ R PG:result -- S PG:result ] -- S ) one answering rows, body on them; PG:REFUSED$ ( -- ptr u8 n ) the last refusal's server text, task-local, until the task's next query. Maki then deletes its copies. Claim: tim (hands to carl).
