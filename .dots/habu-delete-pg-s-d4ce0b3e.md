---
title: "Delete PG's connection-table words nothing calls"
status: open
priority: 2
issue-type: task
created-at: "2026-11-24T20:40:32.343522+01:00"
blocks:
  - habu-connect-to-postgresql-7d9a1131
---

Problem: PG:OPEN, OPEN-START, CONNECTIONS, CONNECTIONS+ and CONNECTIONS-FROZEN (src/lib/pg-rows.f) are reached only by tests. Acceptance: the words and their tests are gone, and dot habu-free-the-pg-d541298d (a halt inside PG:OPEN) is removed with PG:OPEN; the pg suite passes. Depends: the PG password lane (habu-connect-to-postgresql-7d9a1131) lands first, since both edit pg-rows.f OPEN-START. Claim: unassigned.
