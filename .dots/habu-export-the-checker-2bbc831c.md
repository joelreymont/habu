---
title: "Export the checker's cell width for an effect row"
status: open
priority: 2
issue-type: task
created-at: "2026-08-07T11:07:03.357700+02:00"
---

src/core/checker.f publishes an effect as a count of TERMS (EFFECT-DIN-N / EFFECT-DOUT-N) plus a coarse family per term (EFFECT-DIN-FAM / EFFECT-DOUT-FAM, EFAM-GRAY/SCALAR/POINTER/XT). The native chain needs CELLS, because a call site moves cells. The two agree only when every fixed term is EN-CON, EN-PTR or EN-QUOT, each of which is exactly one cell; a GRAY term (EN-VAR, EN-ROW, EN-ATOM, EN-PARAM) has a width the exported projection cannot state. So src/compiler/native/dict.f SPELL-ARITY answers ARITY-NONE for any row carrying a gray fixed term, and a body naming such a word is refused fail-closed rather than compiled against a guessed width. Measured cost: a 'constant' publishes '-- a' and a bare raw type variable is gray, so EVERY named constant is refused - 17.9 percent of lib/'s E-HIR-UNMODELED bucket at the 2026-08-06 census. The checker already computes the right number internally: ROW-CELLS (checker.f:4664) sums ROW-TERM-CELLS over the fixed prefix and knows T-WIDTH, and EFFECT-MIN-IN caps it for the record's min-in byte. It cannot be reused as-is because ROW-CELLS walks the SPA payload representation (R-RES / P>TYPE / P>REST) while the exported query state holds USIGS EN-node offsets (EFFQ-DIN / EFFQ-DOUT, EN.A / EN.B). Fix: publish EFFECT-DIN-CELLS and EFFECT-DOUT-CELLS beside the existing readers, computing width over the EN graph the same way T-WIDTH does over a term, then delete the gray restriction in dict.f and re-run tools/chain-census.f to book the constants tranche.
