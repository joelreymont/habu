---
title: Rename over rows, not cells
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T00:23:52.539857+02:00"
---

PROVED SILENT MISCOMPILE, blocks the cut on its own (2026-08-10, match-design lane): the checker's stack holds ROWS, the elaborator's value vector holds CELLS; an ADT row is more than one cell, so a rename across one permutes the wrong things and nothing refuses because the CELL counts agree. Reproducer (p-miscompile.f in the scratchpad probes dir): ': SWAPOPT ( option<n> n -- n option<n> ) swap ;' - engine returns [n][payload][tag] correctly, chain returns the tag swapped with n and the payload stranded; both compile clean. Exposure: 170 definitions in lib+src name a multi-cell family in their signature, 49 use a rename; the 22 E-NELAB-ARITY census refusals are the visible tip (p-rename-adt.f: 3 of 4 such defs compile, 1 refuses incidentally). FIX IS STRUCTURAL: the elaborator must take term widths from the checker (EFFECT-DIN-N vs EFFECT-DIN-CELLS already distinguish rows from cells) and apply renames over ROWS - or refuse any rename whose window crosses a multi-cell row (refusal acceptable as stage one; silent wrongness is not). Acceptance: SWAPOPT compiles and agrees with the engine value-for-value for both variants; a rename crossing a row boundary either works row-wise or refuses by name; the 49 rename-using ADT definitions all either compile-and-agree or refuse; regression through NMIGRATE:DEFINE executing both publications. Files: src/compiler/native/elaborate.f, maybe chain-census-core.f width readers. Verify: native-elaborate/native-migrate suites, census, full gate. Depends: none. BLOCKS: habu-cut-colon-compilation-a5aa3f1f.

PREMISE CORRECTED 2026-08-10 (by the claiming lane, before building): the
sentence above saying EFFECT-DIN-N vs EFFECT-DIN-CELLS already distinguishes
rows from cells is WRONG for the miscompiling case - a user signature
flattens the bundle into W one-cell terms (measured: option<n> n -- gives
din-n=3 din-cells=3, identical to three scalars), and a user word RETURNING
an ADT is equally invisible on the output side. The fact IS recorded:
checker.f:4498 stores the hidden physical-field slot+1 in EN.E on every
persisted EN-PARAM node (0 = logical term); nothing exports it. The fix
therefore begins with one checker export - EFFECT-DIN-SLOT/EFFECT-DOUT-SLOT
reading EN.E through the existing EFF-ROW-FAM walker - travelling
NDICT:SPELL-ARITY -> HIR-WORD:DECLARE-CALLABLE -> elaborator. RULED (stage
one, this dot): a per-slot bundle marker on the value vector, filled from
din slots at entry and dout slots after each call; RENAME refuses BY NAME
when a consumed window's boundary falls inside a marked run; the refusal
names habu-rename-rows-rowwise (stage two) as the capability it waits for.
Baseline is FOUR miscompiling shapes (swap, rot, and ADT-returning calls
from both generated constructors and ordinary user words), all with
executable differentials in the lane's probes. A definition that compiled
WRONGLY moving to a named refusal counts as an improvement, never a census
regression.
