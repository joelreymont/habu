---
title: "C2c: store a word's facts on its entry"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T20:16:59.583198+02:00"
blocks:
  - habu-c2b-read-facts-f698adb6
---

Problem: design r2 D4, D5, D8: a word's facts text is interned in DATA with its offset in entry bytes 32-39 (DECISIONS 'where the checker's facts live'). Acceptance: CHECK:FACTS! ( ptr u8 n ptr n -- ) and CHECK:FACTS@ ( ptr n -- ptr u8 n ); the checker's CHECK! tail writes each checked word's text; FACTS! parses the text back, caches the parse as the word's effect and refuses the definition when it differs from the proof (E-CONTENT-SAME?); the cache follows each record's [32] (none, cached, re-parsed when [32] moved, dropped when cut) and a failed line restores amended [32]s. Files: src/core/checker.f, src/habu/xref.f, src/habu/layout.f. Verify: candidate build; bin/hb --load test/run.f. Depends: C2b; an installed hb carrying C1 ('Keep names of up to 8 bytes inline'), since a building hb that keeps 16-byte names inline reads [32..40) as name bytes (aot-capture.f ACAP-REC-NAME). Ownership: those files. Claim: unassigned.
