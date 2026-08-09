---
title: Rename over rows, not cells
status: open
priority: 2
issue-type: task
created-at: "2026-08-10T00:23:52.539857+02:00"
---

PROVED SILENT MISCOMPILE, blocks the cut on its own (2026-08-10, match-design lane): the checker's stack holds ROWS, the elaborator's value vector holds CELLS; an ADT row is more than one cell, so a rename across one permutes the wrong things and nothing refuses because the CELL counts agree. Reproducer (p-miscompile.f in the scratchpad probes dir): ': SWAPOPT ( option<n> n -- n option<n> ) swap ;' - engine returns [n][payload][tag] correctly, chain returns the tag swapped with n and the payload stranded; both compile clean. Exposure: 170 definitions in lib+src name a multi-cell family in their signature, 49 use a rename; the 22 E-NELAB-ARITY census refusals are the visible tip (p-rename-adt.f: 3 of 4 such defs compile, 1 refuses incidentally). FIX IS STRUCTURAL: the elaborator must take term widths from the checker (EFFECT-DIN-N vs EFFECT-DIN-CELLS already distinguish rows from cells) and apply renames over ROWS - or refuse any rename whose window crosses a multi-cell row (refusal acceptable as stage one; silent wrongness is not). Acceptance: SWAPOPT compiles and agrees with the engine value-for-value for both variants; a rename crossing a row boundary either works row-wise or refuses by name; the 49 rename-using ADT definitions all either compile-and-agree or refuse; regression through NMIGRATE:DEFINE executing both publications. Files: src/compiler/native/elaborate.f, maybe chain-census-core.f width readers. Verify: native-elaborate/native-migrate suites, census, full gate. Depends: none. BLOCKS: habu-cut-colon-compilation-a5aa3f1f.
