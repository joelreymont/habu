---
title: Let pg handles outlive 2^31 opens
status: open
priority: 3
issue-type: task
created-at: "2026-10-30T20:22:27.899338+02:00"
---

Problem: src/lib/pg.f packs a handle as generation*2^32 + row from one process-wide count with a 31-bit maximum (pg.f:101-102, :491-492) and throws PG:E-CAPACITY (-9257, pg.f:39) once 2^31 handles have been opened in a process, about 25 days at 1000 opens a second. Found by the MCP client re-review (MCP copied the scheme and drops it). Requirement: no fixed limit on opens; a handle value that is the generation alone (63 bits) with the row found by a scan of the generations under the lock, as MCP's client table now does, removing the stride, the maximum and the capacity throw. Verify: pg tests (test/db/), a stale-handle refusal after row reuse. Files: src/lib/pg.f and its tests.
