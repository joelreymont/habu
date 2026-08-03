---
title: Decline the row, not the migration
status: open
priority: 2
issue-type: task
created-at: "2026-08-03T20:53:12.391348+02:00"
---

BLOCKING, destruction review of NINL. ROWS-MAX=64 (src/compiler/native/inline.f:136); ROOM-CK (inline.f:335-337) throws E-NINL-CAP from NMIGRATE:WORK (migrate.f:451) BEFORE REPUBLISH, so once 64 bodies are recorded every later small word fails to MIGRATE at all — proven by probe: 64 rows, then rc=-8575 permanently for every small word (large words still migrate). The file's own rationale (inline.f:132-136) justifies refusing to RECORD a row, not refusing to compile; the other two ceilings (size rule, BODY-MAX) correctly fall back to a call. Fix: at capacity, decline the row deterministically and report it, publish the word normally without a row. Add the missing ROWS-MAX/ROOM-CK test (referenced nowhere in test/). Note: the inline suite itself burns 16 rows, two keyed to fake addresses $20000/$20004 written into the production table (native-inline.f:92-96) — give the suite its own table or clean up.
