---
title: A body edit rewrites every object whose walk reaches the word
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T18:52:55.069874+02:00"
---

Problem: src/habu/linker.f lays a WORDS row for every word a part's reach walk reaches (SEE -> NAMED-REC+, :1209-1210) and WORDS-CK (:3127) refuses the object when that word's time changed. A body edit of CORE-FOLD-C (src/core/util.f) rewrites 109 objects besides util.f's two (81 engine, 27 kernel, hb-build end.f); 108 have bytes equal to a cold build's; 104 never spell the word and reach it through CORE-STR=CI (util.f:54). Evidence: ~/.cache/tmp/carl-why (run.sh, an.py, cmp.py, r1/), hand-off .jj-ws.noindex/carl-why/.out/body-edit-hunt-handoff.md. Not known: per object, the seed and the chain of words that reaches CORE-FOLD-C, and whether the run that seeds it executes the host's baked word or the source being built; whether a changer record's whole-piece span (WALK-STORERS :1272-1278, prefix-boundary.f:28 undefine FIRST-RECORD-XT) is what brings it in. Acceptance: the chain is shown for verify.f+0, checker.f and boot.f+1 by a traced linker; then a design. A warm build after the edit rewrites only objects whose bytes change, warm = cold. Files: src/habu/linker.f. Verify: six-edit census (~/.cache/tmp/carl-rca/six). Depends: none. Ownership: linker.f WORDS rows. Claim: unassigned.

## Chain hunt result (worker, 2026-11-11; traced linker on master aa3a34bf, evidence ~/.cache/tmp/carl-why/t: eng/eng/hb2.log, sum.txt, chain.txt, q4.out; workspace retired, nothing committed)
- On master the edit rewrites 129: util.f's 2 and 127 whose walk reaches CORE-FOLD-C. 4 spell it; of the other 123, 29 reach it by word edges alone and 94 only through a WALK-STORERS whole-piece span.
- Chains: checker.f: seed TOP (its own word, run while it compiles) -> RWIN-NAME -> CORE-STR=CI -> CORE-FOLD-C. boot.f+1: seed `[if]` (util.f) -> CORE-STR=CI -> CORE-FOLD-C. verify.f+0: seed PTR-VARIABLE -> pointer-storage.f extent -> storer span of cell-effects.f -> checker.f extent -> storer span of prefix-boundary.f (holder checker.f row 15) -> FIRST-RECORD -> CORE-STR=CI -> CORE-FOLD-C.
- The seeds run the code just compiled from the source being built, not hb's baked copy (addresses above the last baked record; RAN-RECORD linker.f:1131 drops records below FIRST-REC). So the 29 are true dependencies: a body edit that changed behaviour could change their bytes, and only compiling them shows it did not.
- A three-file program (a.f FOLD and USES, b.f runs USES at build time, c.f calls it at run time): editing FOLD rewrites a.f, b.f and main, not c.f; warm equals cold.
- What remains: the 94 need a store followed only into a reached extent, changer records included (the rejected change vmlukrqw kept the whole-piece rule for header, untimed and changer records, which is why it dropped only 18). That rule is sound only once the language bounds a read to its extent: habu-refuse-an-unbounded-05e30f47, as habu-a-store-brings-1ba604bf records. Not determined: whether the 94 ever execute a word on the far side of a storer span.
- Acceptance as first worded ("rewrites only objects whose bytes change") is not reachable for the 29 without compiling them; it stands for the 94.
