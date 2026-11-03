---
title: Move the string literal store to INTERN
status: closed
priority: 2
issue-type: task
created-at: "2026-11-03T16:03:22.008720+02:00"
closed-at: "2026-11-03T20:13:48.220408+02:00"
close-reason: "landed on master: 8578ad72 Drop the string store's unused words, fd6fd469 Move the string literal store to INTERN (reviewed, fold-ins re-reviewed ACCEPT); full suite on the overnight stack candidate 520 rows, the one red (stdlib-process-fixtures) fixed in efdb0548 and green at tip 77676bcd; two-generation build rc 0 at the tip"
---

Joel 2026-11-03: NSTR (src/compiler/native/string.f) is the string literal store; "call the package INTERN then?" then "do it"; main word INTERN:STRING ("we discussed INTERN:STRING so that one is fine"). Plain move, no critic round (a move is a lane brief): src/habu/intern.f, package INTERN, NSTR-ABI -> INTERN-ABI, E-NSTR-* -> E-INTERN-*, label LINTERNSTRING, test/intern.f; every other word keeps its name. The old cache's ten words stay until the linker deletes the cache (live: tools/native-unit-object.f DEF-ELIGIBLE?). Workspace .jj-ws.noindex/jerry-intern: 9c1ca44c drops the unused words (lead); the move commit on top (worker, brief /tmp/jerry-big/intern-move-brief.md). Baseline: master a0eba663 suite in .jj-ws.noindex/jerry-base, /tmp/jb. Next: COUNT/BYTES (test-only) in a second brief. Claim: agent=jerry.
