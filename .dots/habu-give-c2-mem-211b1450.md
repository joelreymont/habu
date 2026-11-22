---
title: "Give C2-MEM's append header a record"
status: open
priority: 2
issue-type: task
created-at: "2026-11-22T13:59:51.154962+01:00"
---

Problem: src/lib/c2-memory.f lays each append header (next, length, disposer) in allocated bytes and reaches its cells through three casts and offsets (>APPEND-PTR-CELL, >APPEND-LEN-CELL, >APPEND-DISPOSER-CELL; APPEND-NEXT-CELL, APPEND-LENGTH-CELL, APPEND-DISPOSER-CELL; APPEND-HEADER-BYTES 24), and the frame keeps its append-head, append-pending and append-cursor as ^u8. Acceptance: one STRUCTURE append (next ^append, length n, disposer) laid in the bytes through one cast; APPEND-HEADER-BYTES is the record's width; the frame's append links are ^append; the c2 suites pass. Depends: none. Claim: unassigned.
