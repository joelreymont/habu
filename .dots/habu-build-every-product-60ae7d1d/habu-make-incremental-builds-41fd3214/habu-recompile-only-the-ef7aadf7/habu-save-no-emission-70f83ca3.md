---
title: Save no emission number in an entry
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T15:18:31.660726+02:00"
closed-at: "2026-11-10T23:38:05.207969+02:00"
close-reason: its commit of the same subject lands with the word-level stack; suite 273 of 273 and the measure green on the landing tip
---

Problem: a KIND-DEF entry saves its emission's number in the piece and a KIND-DOES entry its definer's (src/habu/linker.f RECORD+, EMISSION+), so a word added mid-file changes the entry bytes of every later definition; each gets a new time and its users recompile. Both are derivable and the check already requires the derived value (EM-NAMED, DOES-CK). Acceptance: entries save neither; EM-NAMED counts, DOES-CK takes LAST-EM, MAKE-DEF takes its emission from the emissions published; ev.sh as before (every rc 0, warm rewritten 0, cold = warm unsigned, restored = cold, probe 3, restore 3, file-objects PASS). Files: src/habu/linker.f, test/fixtures/file-objects/build.f. Verify: ev.sh. Depends: none (independent of the facts lane; rebased onto it at the merge). Ownership: carl. Brief: carl-layout/.out/briefs/lane-entry-no-emission.md. Claim: merged into the stack as surloqst 802ba572 (workspace retired), lands with the feature.
