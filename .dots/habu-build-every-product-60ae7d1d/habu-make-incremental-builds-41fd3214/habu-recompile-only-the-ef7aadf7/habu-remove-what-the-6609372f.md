---
title: Remove what the part machine left
status: active
priority: 2
issue-type: task
created-at: "2026-11-10T16:32:20.588007+02:00"
---

Problem: since habu-link-and-compile-e77aa1cb (a0946130) nothing writes or calls these in src/habu/linker.f, yet they are declared, set up, disposed or branched on: the PENDING buffer, PENDING-HAS?, PENDING? and its eight callers' pending case, ROW-PENDING, FRAMES, PARKED, ROOT-BUILT, WANT-KEY/PIECE/TIME, PIECE-LAY, OBJECTS/OBJECT-DEPTH(S)/OBJECT-AT (OBJECT becomes one buffer), E-STALE (-9692, nothing throws it), and comments naming removed words. Acceptance: a grep of each name in linker.f prints nothing; no behaviour or saved format changes. Files: src/habu/linker.f. Verify: ~/.cache/tmp/carl-words/ev.sh (builds rc=0, warm rewritten 0, cold=warm and restored=cold unsigned, probe rewritten 3, file-objects rc=0). Depends: habu-link-and-compile-e77aa1cb (merged in the stack). Ownership: carl. Brief: carl-layout/.out/briefs/lane-remove-request-leftovers.md. Not in this lane, still to remove after the data lanes that rewrite the same words: BUILT-EM and a BUILT row's two emission cells (no reader); the start input (always 0) of COMPILED-TIME, TIMES+, WORD-TIME, TEXT-SPAN, NEW-SPAN, SAME-SPAN?; GRANTS if a trace shows PRIVS covers it; DECISIONS.md:1540 names E-STALE. Claim: merged into the stack as ozqkmxyo 01ea2425 (workspace retired), lands with the feature. Evidence 2026-11-10 (worker, ev.sh): five builds rc=0, warm rewritten 0, cold=warm and restored=cold unsigned equal, probe rewritten 3, file-objects PASS; linker.f 23 lines added, 75 removed; every listed name is gone (PENDING-PATHS, PENDING-PATH? and DISPOSE-OBJECTS, INIT-OBJECTS, OBJECTS-IN are other words). -9692 is now a gap between E-STORE and E-WORDLIST
