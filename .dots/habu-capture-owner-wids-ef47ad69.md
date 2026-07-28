---
title: Capture owner wids by name
status: open
priority: 2
issue-type: task
created-at: "2026-07-28T18:10:39.886165+02:00"
---

Full context: src/habu/aot-capture.f (OWNER-PAIR@/ACAP-W32@ region, ~lines 374-420) serializes live wordlist ids as raw numerals into the baked OWIDPAIR table. Wordlist ids are host allocation-order numbers, so ANY package opened earlier in the metabuild load renumbers later wids (+2 per package) and shifts those baked u32s — proven by the habu2 packaging lane: seven new packages moved five table entries by exactly +14, a 37-byte image delta (five u32s plus the derived 32-byte signature) with fixpoint, census (4224), CODELEN, and determinism all identical. Effect: semantically-identical packaging edits break image byte-stability, so byte-identity cannot be used as a review invariant for packaging work. Fix: capture package identity by NAME (or normalize wid numbering at freeze) so semantically-identical packaging keeps bin/hb byte-identical. Regression: build twin engines around an added empty package and require identical images.
