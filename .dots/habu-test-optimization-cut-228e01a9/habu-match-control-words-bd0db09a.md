---
title: Match control words ignoring case in the pre-pass
status: open
priority: 2
issue-type: task
created-at: "2026-10-03T04:30:31.268839+02:00"
---

src/habu/verify-source.f WRAP-COND-TOK?, WRAP-LOOP-TOK? and WRAP-BRACKET-TOK? compare control words with CORE-STR= (byte-exact) while the engine and ARM-TOKEN? ignore case. A live target arm ending in uppercase EXIT leaves the loader after its then on the straight line: 'HB-TARGET-MACOS? if s" a.f" required EXIT then s" b.f" required' with a.f and b.f defining the same word loads rc 0 but tools/check.f refuses rc 78 duplicate definition (review 393); a loader under uppercase ?DO ... LOOP is composed (check rc 78, load rc 0). Use STR=CI in the three words, measure WRAP-TOKEN's definer-wrapper rule over the uppercase-style src/habu sources, and add the uppercase-EXIT pick to tools/check-test-lib.f REQ-TARGET-FILES, failing first.
