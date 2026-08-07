---
title: "Export the checker's cell width for an effect row"
status: active
priority: 2
issue-type: task
created-at: "2026-08-07T11:07:03.357700+02:00"
---

src/core/checker.f publishes an effect as a count of TERMS (EFFECT-DIN-N / EFFECT-DOUT-N) plus a coarse family per term (EFFECT-DIN-FAM / EFFECT-DOUT-FAM, EFAM-GRAY/SCALAR/POINTER/XT). The native chain needs CELLS, because a call site moves cells. The two agree only when every fixed term is EN-CON, EN-PTR or EN-QUOT, each of which is exactly one cell; a GRAY term (EN-VAR, EN-ROW, EN-ATOM, EN-PARAM) has a width the exported projection cannot state. So src/compiler/native/dict.f SPELL-ARITY answers ARITY-NONE for any row carrying a gray fixed term, and a body naming such a word is refused fail-closed rather than compiled against a guessed width. Measured cost: a 'constant' publishes '-- a' and a bare raw type variable is gray, so EVERY named constant is refused - 17.9 percent of lib/'s E-HIR-UNMODELED bucket at the 2026-08-06 census. The checker already computes the right number internally: ROW-CELLS (checker.f:4664) sums ROW-TERM-CELLS over the fixed prefix and knows T-WIDTH, and EFFECT-MIN-IN caps it for the record's min-in byte. It cannot be reused as-is because ROW-CELLS walks the SPA payload representation (R-RES / P>TYPE / P>REST) while the exported query state holds USIGS EN-node offsets (EFFQ-DIN / EFFQ-DOUT, EN.A / EN.B). Fix: publish EFFECT-DIN-CELLS and EFFECT-DOUT-CELLS beside the existing readers, computing width over the EN graph the same way T-WIDTH does over a term, then delete the gray restriction in dict.f and re-run tools/chain-census.f to book the constants tranche. Ownership: src/core/checker.f effect-copy + effect-read export API, src/compiler/native/dict.f SPELL-ARITY. Claim: agent=constres workspace=.jj-ws/habu-export-the-checker-2bbc831c.

MEASURED, before and after, with tools/chain-census.f over lib/ and src/ on the
same tree (2026-08-07).

lib/  E-HIR-UNMODELED  1183 -> 350.  The sub-histogram is the real result: 438
distinct refused spellings -> 15, with 424 spellings vanishing outright and
carrying 891 refusals with them.  Every named constant went - MAX-SYMS 32,
E-STR-BOUNDS 21, E-FS-PATH 18, BYTE-LEN>N 14, E-RX-SYNTAX 14, E-PROC-OUTPUT 13,
E-OBJ-SCHEMA 12, E-BUILD-PATH 10, E-FS-CAPACITY 10, STR-ZERO 10, and the long
tail behind them.  What is left is what the dialect genuinely models no
operation for: s" 215, MATCH 54, [: 18, is 16, execute 15, >r 8, case 7, IF 5.

src/  E-HIR-UNMODELED  507 -> 249.  200 distinct spellings -> 10, 190 vanishing
and carrying 299 refusals.  Left: IF 107, s" 65, BEGIN 35, MATCH 19.

AND WHAT DID NOT MOVE, WHICH MATTERS AS MUCH.  'compiled' stayed at 128 in both
trees.  The definitions that now get past name resolution stop at the next
ceiling instead: -8568 E-NCLOB-CAP grew lib 628 -> 1274 and src 166 -> 382.  So
this dot removed the resolution-stage refusal and exposed a capacity one; the
census also mis-classes -8568 as a dialect gap because its code table does not
name it.  Both are dot habu-e-nclob-cap-c981249c.

A SECOND CONSUMER WAS WRONG THE SAME WAY.  tools/chain-census-core.f handed
NELAB:COLON a TERM count as the definition's declared arity, which the
elaborator checks against a vector of CELLS.  Reading the new widths instead
moved lib's declared-arity disagreements 28 -> 19, so at least nine lib
definitions have a fixed row whose cells and terms differ.  Every one of them is
package-private: a top-level probe over the whole engine finds cells == terms
for all 1699 certified records, and only the census, which reopens packages,
reaches the ones that differ.  src/compiler/native/reach.f still reads term
counts and is dot habu-reach-f-reads-070edb9b.
