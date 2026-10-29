---
title: Clear the address maps when a failed evaluate reclaims code
status: open
priority: 2
issue-type: task
created-at: "2026-10-29T21:15:00.914799+02:00"
---

Problem: EM-EVAL-THROW-RECOVER (src/habu/habu2.f:13005-13035) moves CP back after an evaluate throws but leaves CALLMAP/ADDRMAP rows over the reclaimed span (xref.f TRUNCATE :765-776 clears them); a later snapshot save fails 'snapshot address map mismatch' rc 97. Repro (/tmp/jerry-b1-repl/tree/probe/p4.f, public hb): a failed evaluate of ': FOO ( -- ) ['] DUP drop ; 99 throw' then a REPL save; both controls (no throw, no tick) save and run. Acceptance: the save succeeds after a failed evaluate; one test row with that program through the real save path. Files: src/habu/habu2.f (inside the JIT transport region 11387-14300: check whether the path survives habu-delete-the-jit-d9fdaf67 before fixing). Verify: the new row; the test/compiler rows that save REPL images. Depends: none. Ownership: src/habu/habu2.f. Claim: unassigned.
