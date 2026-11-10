---
title: Check a referenced file by its words
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T12:23:01.375089+02:00"
---

Problem: once objects list the words they refer to (WORDS rows), a FILES row a WORDS row covers still carries the piece's time, so the piece's every change still recompiles the file. Acceptance: REF-FILE takes whole or not; a FILES row made only by record references (REF+) to a piece built before the root file's load began has time 0 and matches a built piece by key and number (BUILT-MATCH, PENDING-HAS?); rows from CODE-REF+, DATA-REF+, REACHED, NAMED+, the request row and every piece built since the root's load began stay whole; a words-only row on a pending piece is refused. Fixture sequence: a.f two words, b.f calls one, c.f the other, d.f aliases the first, e2.f calls the alias; a body edit of the first compiles a, b, d, e2 and links c. Then: repl.f + unused word rewrites fewer than 21 objects. Files: src/habu/linker.f, test/file-objects.f, test/fixtures/file-objects/build.f. Verify: file-objects; cold/warm cmp; probe count. Depends: habu-count-rows-that-1e6ebb3f and 'List the words an object refers to' merged. Then separately: whole rows for every piece built inside a file's own load (design 3.5), and a crafted bad WORDS row. Ownership: carl. Claim: unassigned.

Lane (carl, 2026-11-10): workspace carl-cw, brief .out/briefs/lane-check-by-words.md. Corrected from the code before the brief: a file's source names every piece that defines a name it spells with a whole row (linker.f NAMED+, HIT marks every public name with the spelled sum, whatever its package), so no fixture file and almost no engine file gets a words-only row until names are tracked by word. The a.f/b.f/c.f/d.f/e2.f sequence and "repl.f + unused word rewrites fewer than 21" therefore move to the names lane (draft-words-by-reference.md "Lane B"); this lane's evidence is the unchanged ev.sh lines and a count of time-0 FILES rows. Added to the draft's rule: a words-only row that is not built at WORDS-CK is refused whether pending or absent, since a link under a linking file would otherwise end the build E-STALE; REF+ also makes the row whole when the word has no time.

2026-11-10: merged into the stack as louplnox "Check a referenced file by its words" (carl-cw retired). ev.sh: cold 13.2 s, warm 1.6 s, probe 21, cold = warm = restored unsigned, file-objects PASS. All 202 time-0 FILES rows are the build body's row (key "", piece 0, time 0 by OWNED-RECORDS), whole rows: a count of words-only rows leaves that row out.
