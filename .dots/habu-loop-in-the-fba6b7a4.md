---
title: +loop in the engine JIT terminates at the boundary opposite the limit
status: closed
priority: 2
issue-type: task
created-at: "2026-09-14T16:38:13.789340+03:00"
---

Resolved 2026-09-14 at 1215c0be after Cedar's independent review. The JIT and
bootstrap mirror now use the directed crossing rule. Eleven bounded cases pass
on the combined loop engine SHAd5d84808 and on the isolated Gforth recovery
output. The implementing lane also ran engine-suite and clobber lint. Full
compiler qualification and the separate public recovery failure remain open.

Problem: src/habu/habu2.f J-+LOOP tests only whether (index - limit) changed sign across the step (sub x15,x13,x10; add x13,x13,x9; sub x16,x13,x10; eor x15,x15,x16; cmp x15,#0; b.ge loop-top), so a wrap at the boundary opposite the limit also ends the loop. Forth 2012 6.1.0140 ends the loop only when the index crosses the limit-1|limit boundary in the step's direction: terminate iff ((old xor next) and (old xor step)) < 0. Reproduced on the legacy-JIT engine with two-turn leave guards: limit 0 start MAX-INT step 1 returns 1 (expected 2), limit 0 start MIN-INT step -1 returns 1 (expected 2); equal-bound -1 -> 1, equal-bound +1 -> 6 (guard), 4 1 1 -> 3 are right. Acceptance: with x9 still holding the step, replace the sign-only test by eor x16,x15,x16 (old xor next), eor x15,x15,x9 (old xor step), and x15,x15,x16, then the existing cmp/b.ge; add engine-tier regressions for the two opposite-boundary cases (two-turn guard), the standard's gd8 large-increment rows (256 turns) and equal bounds per step sign; the native chain's DO-CLOSE-LOOP-STEP (dot habu-lower-dynamic-counted-db5978a3) already uses the directed rule and its fixture test/compiler/native-plusloop.f carries the expected cases. Files: src/habu/habu2.f (J-+LOOP), an engine-tier fixture under test/. Verify: rebuild the engine cold and run the fixture plus test/engine-suite.f. Depends: none. Ownership: engine stack owner (assigned by cedar), isolated commit first. Claim: unassigned.
