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
From the shim review (2026-11-06, /tmp/jerry-rev8): four places still treat [32..40) as zero name padding, true while no writer stores facts there, and each would erase or drop facts once C2c stores them: src/habu/habu2.f NAME-COPY (:3598) zeroes [24] and [32] when a word is defined; habu2.f EM-AOT-REGISTER-RECS (:6816) zeroes [24..40) on boot for every shipped row (the 20-byte row has no field to carry them); src/habu/habu1.f EMIT-DICT (:6280) pads inline names to 16 bytes; src/habu/aot-capture.f :298 says "the [24..40) name padding". Also src/habu/layout.f:127-128 says a `;does` clause's record is out of line, but C-EXPORT-CLAUSE-RECORD (habu2.f:10427) stores an exported clause name through NAME-COPY, inline without DNAME-EXT for a 6-8 byte name: limit that comment to DOES-REC's own record.
