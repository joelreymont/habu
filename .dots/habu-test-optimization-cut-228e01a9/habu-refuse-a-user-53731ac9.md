---
title: Refuse a user X;does before X does>
status: open
priority: 2
issue-type: task
created-at: "2026-10-01T17:24:58.103796+02:00"
---

Problem: the duplicate wall is asymmetric for a definer's clause record. `: MKV;does ( -- n ) 5 ;` then `: MKV ... does> ... ;` loads with both rows live ($HOME/.cache/tmp/kestrel-r4-rev178/dup-before.f): MKV;does answers the user's 5 while a word made by MKV runs the clause (7); the reverse order is refused rc 78 (dup-after.f). habu1.f:3225-3245 states the hash probe's precondition (at most one live row per folded name per wid, kept by C-REJECT-DUP-DEFINITION), and J-DOES publishes the clause without that wall (aot-capture.f:1658); the AOT seed resolves the clause by name in another engine (habu2.f:3305), so it would answer the user's word. Found by review 178 (acap-redef). on master 9751f482 the engine side has landed: DOES-REC:REJECT-DUP (src/habu/habu2.f:3786), called by J-DOES and NCOMP-EMIT:CAPTURE-DOES (:9152), refuses the definer at `does>` with rc 78 `duplicate definition: MK;does`, at tier 0 and in the engine loop at tier 1 (located `at <path>:2`), as docs/forth.md:366-369 and docs/forth-card.md:41-43 state. The Habu loop still admits it at tier 1: its `does>`, DEF-DOES in src/habu/definers.f:366-369, refuses only a second `does>` and never looks up `<name>;does`, so ~/.cache/tmp/carl-gfrest/c/g2-e-dupname-c.f and g2-e-dupname-t.f print 5, rc 0, and g2-dup-before.f (the dot's dup-before.f) leaves both rows live (`user MKV;does=5 SEVEN=7`), under `bin/hb --load test/outer-loop-on.f <file holding 1 set-tier> <case>`. tools/check.f refuses each, rc 78. The reverse order is refused by the Habu loop too (dup-after.f, rc 78).
Acceptance: under the Habu loop at tier 1, DEF-DOES refuses as CAPTURE-DOES does, before the signature is read: the three reproducers exit 78 with `duplicate definition: <NAME>;does at <path>:<line>`, as the engine loop prints; the same name in another wordlist refuses nothing. The reproducers join test/outer-interpret.f beside EXPORT-DEFINER's clause-held case (:1013-1014).
Files: src/habu/definers.f, test/outer-interpret.f.
Verify: each reproducer under both loops at tier 1; `bin/hb --load test/outer-interpret.f`; `bin/hb --load test/does-clause-record.f`.
Depends: none.
Worker: worker.
