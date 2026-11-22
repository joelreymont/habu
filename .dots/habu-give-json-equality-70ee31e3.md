---
title: Give JSON equality typed tables
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T12:52:51.576693+01:00"
---

Problem: src/lib/json/equal.f lays VALUE='s rows, member list, spare and frames out by hand in one byte tail of its WITH-BYTES block, moved to a raw mapping when it outgrows it (TAIL-0, MOVE-TAIL, FREE-TAIL), and reaches rows and frames by casting tail bytes (CAST: >ROW and >FRAME, ROW and FRAME at :295-310). Acceptance: rows, frames and the member list are typed storage the walk owns (vectors of row and frame records, or equal), no cast from tail bytes remains, VALUE= keeps its n log n bound and its no-move case for small texts, and tail-pure-fixtures passes. Depends: habu-let-lib-tokens-e95db608/habu-let-a-json-6dc4d207 (moves the walk itself into a lent record). Claim: unassigned.
