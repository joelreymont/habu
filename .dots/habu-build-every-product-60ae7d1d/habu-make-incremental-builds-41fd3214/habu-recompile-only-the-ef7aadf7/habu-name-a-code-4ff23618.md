---
title: "Name a code reference's record by its identity"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T15:00:48.279007+02:00"
closed-at: "2026-11-10T23:38:05.090689+02:00"
close-reason: its commit of the same subject lands with the word-level stack; suite 273 of 273 and the measure green on the landing tip
---

Problem: a reference to a record from code or a data cell is saved as tag 0, a file and the record's number in that file's piece (src/habu/linker.f REF+, RD-REF, CODE-REF-CK, RESOLVE through REC-INDEX), so a word added mid-file changes the saved bytes of every word that calls a later word, which gets a new time, and its users recompile (six-edit run3: 166 objects for a word added mid-file in src/core/util.f). Acceptance: tag 0 saves the file, the record's name sum and its count among the file's records with that sum; the check and the link find an own record through the piece's TIMES identities and an earlier one through PLACE; ev.sh as before (every rc 0, warm rewritten 0, cold = warm unsigned, restored = cold, probe 3, restore 3, file-objects PASS). The six-edit counts do not move until entry references, facts references, emission numbers and data references follow. Files: src/habu/linker.f, test/fixtures/file-objects/build.f. Verify: ev.sh. Depends: habu-name-a-word-cf39c892 with its per-file fold-in. Ownership: carl. Brief: carl-layout/.out/briefs/lane-code-refs-by-identity.md. Claim: agent=carl; merged into the stack as swowvqum 3d69dfa3 (workspace retired), lands with the feature.
