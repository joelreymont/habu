---
title: List the words a source spells
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T12:52:24.879451+02:00"
closed-at: "2026-11-10T23:38:05.065094+02:00"
close-reason: its commit of the same subject lands with the word-level stack; suite 273 of 273 and the measure green on the landing tip
---

Problem: a file depends whole on every earlier piece that defines a name its source spells (src/habu/linker.f NAMED+, NAMED-CK), so a body edit of one word recompiles every file spelling any name of its piece. Acceptance: a spelled name is a WORDS row for the one record; file-objects' words sequence prints 'words: compiled 5 4 0 2 1 1 0' and every other sequence's line is unchanged; warm = cold unsigned. Files: src/habu/linker.f, test/file-objects.f, test/fixtures/file-objects/build.f. Verify: ev.sh on the lane workspace, file-objects PASS, no time-0 FILES row unnamed by WORDS. Depends: habu-check-a-referenced-5c63194b (merged in the stack). Ownership: those files. Brief: carl-layout/.out/briefs/lane-names-by-word.md. Claim: agent=carl workspace=.jj-ws.noindex/carl-nm

2026-11-10: merged into the stack as mvlkqltv (carl-nm retired). ev.sh: cold 13.0 s, warm 1.7 s, probe 2.4 s; repl.f + PROBE-X rewrites 11 objects (21 before); cold = warm = restored unsigned; file-objects PASS with `words: compiled 5 4 0 2 1 1 0`; 1699 words-only FILES rows, none unnamed by WORDS. The 10 objects besides repl.f that the probe rewrites all list repl.f in REACHED (the run rule); 6 that only name its words link. For src/core/util.f: 190 objects reach it by the run rule, 10 name its words only (~/.cache/tmp/carl-words/an/why.py).
