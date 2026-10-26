---
title: Scope PG results with a linear token
status: open
priority: 2
issue-type: task
created-at: "2026-10-27T21:26:54.747283+02:00"
---

Problem: a PG result was a copyable nominal handle that a caller could keep past CLEAR or let escape its use; only a runtime generation check caught it. Acceptance: PG:result is a DEFLINEAR token; WITH-RESULT, WITH-SCRIPT, WITH-PREPARE, WITH-PREPARED and WITH-DESCRIPTION hand the body the token and clear the result on every path; accessors (OUTCOME ROWS COLS AFFECTED NAME$ OID TABLE TABLE-COL TEXT$ NULL? INT) thread the token; CLEAR consumes only the POLL/AWAIT completed token; failed spans read the result in place; DB-ROWS migrates to the scoped model: BY-ID is a scope over WITH-RESULT that checks the first row and runs a body on the result, WITH-ROW is removed, the COL- and AT readers thread the token and ONE-INT runs its query in its own scope, with the arena unchanged; in-repo consumers (pg-test, pg-hold and rows-test) migrate. Verification: test/db/pg-cluster.f (pg-test.f and src/lib/db/rows-test.f) green; tools/check.f loads test/db/pg-hold.f.
