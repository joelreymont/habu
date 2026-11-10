---
title: List the words an object refers to
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T12:23:01.361992+02:00"
closed-at: "2026-11-10T23:38:05.077519+02:00"
close-reason: its commit of the same subject lands with the word-level stack; suite 273 of 273 and the measure green on the landing tip
---

Problem: an object names each earlier piece it refers to by a FILES row with the piece's time (linker.f REF-FILE, BUILT-MATCH), so a change anywhere in a piece recompiles every file that refers to any word of it. Words have their own times (TIMES, WORD-TIMES) and nothing reads them. Acceptance: the object holds a WORDS section between PATHS and TIMES, a row (FILES row, word number, word time) per earlier record REF+ references, each once; HEADER-CK refuses the object when a row's word is not in this build with that time; FILES rows keep their time (the following lane, 'Check a referenced file by its words', drops it for rows WORDS covers). Brief: .jj-ws.noindex/carl-layout/.out/briefs/lane-words-rows.md. Files: src/habu/linker.f, test/file-objects.f, test/fixtures/file-objects/build.f. Verify: cold/warm engine cmp, 0 rewritten, file-objects; build times against 12.8 s cold, 1.6 s warm. Depends: fold-ins merged in carl-words. Ownership: carl. Claim: unassigned.
