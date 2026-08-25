---
title: Materialise a double with fewer instructions
status: open
priority: 2
issue-type: task
created-at: "2026-08-02T23:04:01.326091+02:00"
---

src/compiler/native/select.f EMIT-FCONST builds a double literal the way the engine does: the move-wide chain into a general register, then one FMOV across (one to five instructions). AArch64 also has FMOV with an eight-bit immediate, which reaches 256 doubles in one instruction, and a literal pool load reaches every double in two plus a pool the emission does not have. Which is cheaper is a measurement against tools/codegen-compare-corpus3.f's committed table, not a guess: the pinned outputs are bit-exact either way, so what would move is the byte count and the cost. Do it with the table in hand.
