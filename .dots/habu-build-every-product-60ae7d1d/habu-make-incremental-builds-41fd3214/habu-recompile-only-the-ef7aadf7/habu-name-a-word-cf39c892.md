---
title: Name a word by its name sum and count in the piece
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T14:20:34.788832+02:00"
---

Problem: a word added mid-file renumbers every later word of its piece, and the linker names a word by that number in three places: src/habu/linker.f WORD-TIME :3302 compares a word with the old object's word of the same number, so every later word gets a new time; WORD-ROW+ :686 saves the number in a dependent's WORDS row and WORDS-CK :2599 reads it. Six-edit run3: an unused word or variable added mid-file in src/core/util.f rewrites 166 objects. This lane is the first of the identity-by-name steps (design-word-level.md 3.4 Step B): a word's identity is its name's NAME-SUM and its count among the piece's records with that sum; TIMES saves it per word, WORD-TIME finds the old word by it, WORDS rows carry it and WORDS-CK resolves it through a per-build table. References inside PIECES (tag 0, tag 2, entries' emission numbers) stay numbers until the next lanes, so the six-edit counts do not move yet. Acceptance: ev.sh as before (every rc 0, warm rewritten 0, cold = warm unsigned, restored = cold, probe 3, restore 3, file-objects PASS); a scratch check that every word of a cold engine build finds itself by identity. Files: src/habu/linker.f, test/fixtures/file-objects/build.f. Verify: ev.sh; the scratch check. Depends: none. Ownership: carl. Brief: carl-layout/.out/briefs/lane-word-identity.md. Claim: agent=carl workspace=.jj-ws.noindex/carl-idn

Fold-in 2026-11-10 (brief .out/briefs/fold-word-identity-file.md, worker in carl-idn amending f8d89de0): the count runs over the file, not the piece, and PLACES is keyed by the file key (FILE-KEY, which every BUILT and FILES row starts with), so a dependent never names a piece; one PLACE-SLOT word replaces the three spelled probes. Waiting case, not in this lane: a word that moves to another piece because a request was added before it gets a new time, since OLD-WORD searches only the old piece of the same number.
